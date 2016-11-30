

# Define server logic required to draw a histogram
shinyServer(
  
  function(input, output, session) {
  
    #________________Getting all the uploaded data_______
    
    #___________Handling data format for the MARSS package
    
    makeList<- function(data){
      l<-list()
      for ( i in 1:length(data)){
        l[[toString(data[i])]]<-data[i]
      }
      return(l)
    }
    
    
    # _____________________Data table input from the ui_____________________
    
    data<-reactive({
      inFile<- input$data
      req(inFile)
      table<-read.csv(inFile$datapath)
      vars<-names(table)
      vars1<-makeList(vars)
      
      #Updating the ui with all the parameters availiable from the table
      updateSelectInput(session, "paramsAutoArima","Select Columns:", choices = vars)
      updateSelectInput(session, "paramsState","Select Columns:", choices = vars)
      updateSelectInput(session, "paramsDlm","Select Columns:", choices = vars)
      updateSelectInput(session, "explainDlm","Choose explanatory variable:", choices = vars)
      updateSelectInput(session, "paramsArima", "Select data:", choices = vars)
      updateCheckboxGroupInput(session, inputId="xregParamsArimax", choices = vars, selected=NULL)
      updateCheckboxGroupInput(session, inputId="paramsMARSS", choices = vars, selected=vars[1])
      table
    })
    
    #________________Outputting the uploaded table___________

    
    output$contents <- DT::renderDataTable({  
      DT::datatable(data())
    })


    ####################### Arima ###############################
    
    
    #_________Reading the user input__________
    xreg<- reactive({
      input$xregParamsArimax
    })

    
    #________Fitting an Arima model ______________
    arimaFit<-reactive({
      
      inFile<- data()
      
      if (input$arimaModel == "Manual"){
        
        params<-input$paramsArima
        data1<-inFile[, params]
        x<-input$arimaOrder
        x<-as.numeric(unlist(strsplit(x, ",")))
        if (is.null(xreg())){
          model<-Arima(data1, order = x)
        } else {
          model<- Arima(data1, order = x, xreg = inFile[, xreg()])
        }
        
      } else {
        
        data1<-inFile[, input$paramsAutoArima]
        model<- auto.arima(data1)
      }
      
      model
    })
    
    
    
    #____________ Forecasting the sata with Arima_________
    
    arimaForecast<- reactive({
      if (is.null(xreg())){
        
        f<- forecast(arimaFit(), h=input$arimaPeriod)
      } else{
        f<-forecast(arimaFit(), h=input$arimaPeriod, xreg=data()[,xreg()])
      }
      f
      
    })
    
    
    
    logTextArima <- reactive({
      capture.output(arimaFit())
    })
    
    
    
    
    #______Construction forecast output_________
    
    observeEvent(input$analyseArima, {
      
      output$console<- renderPrint({
        
        logTextArima()
        
      })
      
      
      output$arimaForecast<- DT::renderDataTable({
        
          #table<- data.frame(MARSSsimulate(marss(), tSteps=input$period)$sim.data)
          #table<-t(table)
       
          table<- data.frame(arimaForecast())
          colnames(table)<-c("Forecast", "Low 80", "High 80", "Low 95", "High 95")
          DT::datatable(table)
      })
      
      
      output$arimaPlot<-renderPlot({
        
        plot(arimaFit()$x)
        
      })
      
      output$arimaForecastPlot<-renderPlot({
        
        plot(arimaForecast())
        
      })
      
    })
    
    
    #_____________Creating conditional panel_________

    
  #  output$fileUploaded <- reactive({
     # return(!is.null(data()))
    #})
    
    #outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    
    #_______________Warnings handler_________________
    
    output$warn <- reactive({
      return(length(warning())>1)
    })
    
    outputOptions(output, 'warn', suspendWhenHidden=FALSE)

    
  #__________ Space-State modeling
  
  state<-reactive({
    
    inFile<- data()
    data1<-inFile[, input$paramsState]
    type<-input$StateType
    StructTS(data1, type=type)
    
  })
  
  
  #_______ Linear regression space state model with constant coefficients
  buildModRegConst<-function(v){
    ##########################
    ###TODO: 4 parameters input handler
    ############################
    dV<- exp(v[1])
    dW<- c(exp(v[2]), 0)
    m0<- v[3:4]
    data<-data()
    x<- data[, input$explainDlm]
    dlmModReg(x, dV= dV, dW=dW, m0=m0)
    
  } 
  
  #_______ Linear regression space state model with time varying coefficients
  buildModRegVariant<-function(v){
    ##########################
    ###TODO: 4 parameters input handler
    ############################
    dV<- exp(v[1])
    dW<- c(exp(v[2:3]))
    m0<- v[4:5]
    data<- data()
    x<- data[, input$explainDlm]
    dlmModReg(x, dV= dV, dW=dW, m0=m0)
    
  }  
  
  
  #_________Constructing manual dlm_________________
  
  buildDlmPoly<- function(x){
    n<- input$dlmPolyOrder
    rw <- rw <- dlmModPoly(n, dV=x[1], dW=x[2], C0=x[3], m0=x[4])
    return(rw)
  }
  
  buildDlmSeas<- reactive({
    if (input$seasType=="Seasonal"){
      dlmModSeas( frequency= input$seasFreq, dV=input$seasVarNoise, dW=input$seasVarSys)
    } else if (input$seasType=="Fourier form"){
      dlmModTrig(q=input$trigHarmonics, tau=input$trigPeriod, dV=input$trigVarNoise ,dW=input$trigVarSys)
    }
  })
  
  
  
  #____________Calculating startign values for the optim in dlm()
  initGuessParams<- reactive({
    
    inFile<- data()
    data<-inFile[, input$paramsDlm]
    varGuess<- var(diff(data), na.rm=TRUE)
    mu0Guess<-data[1]
    lambdaGuess<-mean(diff(data), na.rm=TRUE)
    if (input$typeDlm=="Time-varying coefficients"){
      params<-c(log(varGuess), log(varGuess/5), log(varGuess/5), mu0Guess, lambdaGuess)
      mle<- dlmMLE(data, parm = params, build = buildModRegVariant,  method = "Nelder-Mead") 
    } else if (input$typeDlm=="Manual"){
      params<- c(log(varGuess), log(varGuess/5), mu0Guess, lambdaGuess)
      mle<- dlmMLE(data, parm = params, build = buildDlmPoly,  method = "Nelder-Mead")
    } else {
      params<- c(log(varGuess), log(varGuess/5), mu0Guess, lambdaGuess)
      mle<- dlmMLE(data, parm = params, build = buildModRegConst,  method = "Nelder-Mead")
    }
    mle
    
  })
  
  
  #___________Updating a size of input in the ui depending on the type of space-state model 
  observe({
    if (input$typeDlm=="Time-varying coefficients"){
      updateTextInput(session, "dlmParams", "Choose parameters for the model:", "0,0,0,0,0")
    } else {
      updateTextInput(session, "dlmParams", "Choose parameters for the model:", "0,0,0,0")
    }
    
  })
  
  
  #__________Constructing a dlm model with the paramteres
  dlm<-reactive({
    
    x<-input$dlmParams
    x<-as.numeric(unlist(strsplit(x, ",")))
    
    init<-initGuessParams()
    if (x != c(0,0,0,0) || x!= c(0,0,0,0,0)){
      params<- x
    } else{
      params<- init$par
    }
    
    if (input$typeDlm=="Time-varying coefficients"){
      dlm<- buildModRegVariant(params)
    } else if (input$typeDlm=="Manual"){
      
      if (input$seasType=="No seasonality"){
        dlm<- buildDlmPoly(params)
      } else {
        dlm<- buildDlmPoly(params) + buildDlmSeas() 
      }
   
    } else {
      dlm<-buildModRegConst(params)
    }
    dlm
    
  })
  
  
  #__________Smoothign the data usign the dlm model
  smoothDlm<- reactive({
    inFile<- data()
    data<-inFile[, input$paramsDlm]
    model<- dlm()
    smooth<-dlmSmooth(data, model)
    smooth
  })
  
  
  #___________Kalman Filtering for the dlm model
  filterDlm<-reactive({
    inFile<- data()
    data<-inFile[, input$paramsDlm]
    model<- dlm()
    fil<-dlmFilter(data, model)
    fil
    
  })
  
    #__________MARSS handler__________
    
    MARSSHandler<-reactive({
      
      inFile<- input$data
      req(inFile)
      table<-read.csv(inFile$datapath)
      
      dat<-table[, input$paramsState]
      
      dat<-sapply(dat, as.numeric)
      dat<-as.matrix(dat)
      dat<-t(dat)
      dat
      
    })
    
    marss<-reactive({
      dat<-MARSSHandler()
      mars<-MARSS(dat)
      mars
    })
    
    observeEvent(input$analyseState,{
      
      
      modelState<- reactive({
        
        model<- switch(input$StateModel,
                       "dlm"={
                         dlm()
                       },
                       "Structural"={
                         state()
                       },
                       "Autoregressive"={
                         marss()
                       })
        model
  
      })
      
      
      output$stateModel<- renderPrint({
        
        model<- modelState()
        capture.output(model)
        
      })
      
      logTextState <- reactive({
        capture.output(modelState())
      })
      
      output$consoleState<- renderPrint({
        logTextState()
      })
      
      output$stateDiag<-renderPlot({
        
        if ( input$StateModel=='Structural'){
          tsdiag(modelState())
        } else{
          tsdiag(filterDlm())
        }
      })
      
      stateForecast<- reactive({
        
        
        if ( input$StateModel=="Structural"){
          data<-state()
           f<-forecast(data, h= input$statePeriod)
        } else if (input$StateModel=="dlm"){
          data<- filterDlm()
          f<- dlmForecast(data, nAhead = input$statePeriod, sampleNew=input$statePeriod)
          
        }
        f
        
      })
      
      
      output$stateForecast<- DT::renderDataTable({
        
        if (input$StateModel=="Structural"){
          table<- data.frame(stateForecast())
          colnames(table)<-c("Forecast", "Low 80", "High 80", "Low 95", "High 95") 
        } else {
          data<-stateForecast()
          table<-data.frame(unlist(data$f), data$newObs)
          colnames(table)<-rep("Forecast obs", input$statePeriod+1)  
        }
        DT::datatable(table)
        
      })
      
      
      sortDlmForecast<- function(x){
        
      }
      
      output$stateFittedPlot<- renderPlot({
        
        if (input$StateModel=="dlm"){
          data1<- data()
          data<-data1[, input$paramsDlm]
          filtered<- dlmFilter(data, dlm())
          smooth<- dlmSmooth(data, dlm())
          if (input$typeDlm=="Manual"){
            if ( input$seasType=='No seasonality'){
              filt<-dropFirst(filtered$m)
              smoothed<-dropFirst(smooth$s)
            } else {
              filt<-dropFirst(filtered$m[,1])
              smoothed<-dropFirst(smooth$s[,1]) 
            }
          } else {
            x<- data1[, input$explainDlm]
            filt<- filtered$m[-1,1]+ x* filtered$m[-1,2]
            smoothed<- smooth$s[-1,1]+ x* smooth$s[-1,2]
          }
          plot(data, type="o", pch=19, bg="black")
          lines(filt ,lty = "dashed", lwd = 2, col="red")
          lines(smoothed,lty = "dotted", lwd = 2, col="blue")
          legend("bottomright", legend = c("Data", "Filtered","Smoothed"), lty = c(1,2,3), pch=c(19, NA, NA), col=c("black", "red", "blue"), lwd=c(1,2,2))
          
        } else {
          data<-state()
          data1<- data()
          data1<-data1[, input$paramsState]
          
          plot(data1, type="o", pch=19, bg="black")
          lines(fitted(data)[,1],lty = "dashed", lwd = 2, col="red")
          lines(tsSmooth(data)[,1],lty = "dotted", lwd = 2, col="blue")
          legend("bottomright", legend = c("Data", "Filtered","Smoothed"), lty = c(1,2,3), pch=c(19, NA, NA), col=c("black", "red", "blue"), lwd=c(1,2,2))
        }
        })
      
      output$stateForecastPlot<-renderPlot({
        
        if (input$StateModel=="dlm"){
          data1<- data()
          data<-data1[, input$paramsDlm]
          forecast<-stateForecast()
          plot(c(rep(NA, length(data)),forecast$f), type = 'o', lwd = 2, pch = 16, ylab = "Data")
          lines(data,type = 'o')
          invisible(lapply(forecast$newObs, function(x) lines(c(rep(NA,length(data)),x), col = "darkgrey",
                                                              type = 'o', pch = 4)))
          abline(v = length(data)+0.5, lty = "dashed")

          
        } else {
          plot(stateForecast()) 
        }
      })
    })
    
    
  })


