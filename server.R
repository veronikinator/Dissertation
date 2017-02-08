

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
      updateCheckboxGroupInput(session, inputId="paramsData", choices = vars, selected=vars)
      table
    })
    
    #________________Outputting the uploaded table___________

    
    output$contents <- DT::renderDataTable({  
      DT::datatable(updatedData())
    })
    
    updatedData<- reactive({
      data()[,input$paramsData]
    })
    
    output$dataPlot<- renderDygraph({
      dygraph(updatedData()) %>%
        dyRangeSelector()
    })


    ####################### Arima ###############################
    
    
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
    
    
    xreg<- reactive({
      input$xregParamsArimax
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
    
    
    #_______________Warnings handler_________________
    
    output$warn <- reactive({
      return(length(warning())>1)
    })
    
    outputOptions(output, 'warn', suspendWhenHidden=FALSE)

    
  ##################################Space-State modeling#########################
  
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
  
  #buildDlmPoly<- function(x){
   # n<- input$dlmPolyOrder
   # rw <- dlmModPoly(n, dV=x[1], dW=c(rep(x[2], n - 1), 1), C0=x[3]*diag(nrow = n), m0=rep(x[4], n))
   # return(rw)
 # }
  
  buildDlmPoly<- function(p){
    #n <- input$dlmPolyOrder
    n<- input$dlmPolyOrder
    rw <- dlmModPoly(n, dV=p[1], dW=c(rep(p[2], n - 1), 1), C0 = p[3]*diag(nrow = n), rep(p[4], n))
    return(rw)
  }
  
  buildDlmSeas<- reactive({
    if (input$seasType=="Seasonal"){
      dlmModSeas( frequency= input$seasFreq, dV=input$seasVarNoise, dW=input$seasVarSys)
    } else if (input$seasType=="Fourier form"){
      dlmModTrig(q=input$trigHarmonics, tau=input$trigPeriod, dV=input$trigVarNoise ,dW=input$trigVarSys)
    }
  })
  
  fitDlmPoly<- function(p, data){
    mod<- dlmMLE(data, parm = p, build = buildDlmPoly )
    return(mod)
  }
  
  
  
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
  
  
 buildDlmMannual<- function(params){
   
 }
  
  
  #__________Constructing a dlm model with the paramteres
  dlm<-reactive({
    
    #x<-input$dlmParams
    dv<-as.numeric(input$dlmParamsDv)
    dw<- input$dlmParamsDw
    dw<-as.numeric(unlist(strsplit(dw, ",")))
    m0<-input$dlmParamsM0
    m0<-as.numeric(unlist(strsplit(m0, ",")))
    c0<-input$dlmParamsC0
    c0<-as.numeric(unlist(strsplit(c0, ",")))
    #x<-as.numeric(unlist(strsplit(x, ",")))
    
    init<-initGuessParams()
    inFile<- data()
    data<-inFile[, input$paramsDlm]
    p<- c(dv,dw, c0, m0)
    
    
    
    #if (x != c(0,0,0,0) || x!= c(0,0,0,0,0)){
  #    params<- x
#    } else{
 #     params<- init$par
  #  }
    
    if (input$typeDlm=="Time-varying coefficients"){
      dlm<- buildModRegVariant(params)
    } else if (input$typeDlm=="Manual"){
      
      if (input$seasType=="No seasonality"){
        #dlm<- buildDlmPoly(params)
       # dlm<- buildDlmPoly1(n=n, dv= dv,dw= dw, c0=c0, m0=m0)
        pars<- fitDlmPoly(p, data)
        dlm<- buildDlmPoly(pars$par)
      } else {
        #dlm<- buildDlmPoly(params) + buildDlmSeas() 
        #dlm<- buildDlmPoly1(n = n, dv= dv,dw= dw, c0=c0, m0=m0) + buildDlmSeas() 
        dlm<- fitDlmPoly(p, data) + buildDlmSeas() 
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
      
      dat<-table[, input$paramsMARSS]
      
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
  
  marssSmooth<- reactive({
    model<-marss()
    s<-MARSSkfss(model)$xtT
    s
  })
  
  marssKalman<- reactive({
    model<-marss()
    k<-MARSSkfss(model)$xtt
    k
    })
  
  marssForecast<- reactive({
    f<-stateForecast()
    forec<-data.frame(f$sim.data)
    forec2<-data.frame(f$sim.states)
    a<-data.frame(t(forec))
    b<-data.frame(t(forec2))
    table<- data.frame(a,b)
    table
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
        } else if (input$StateModel=='dlm'){
          tsdiag(filterDlm())
        } else {
          NULL
        }
      })
      
      
      dlmPredict<- reactive({
        
        data<- filterDlm()
        
        if (input$typeDlm=='Manual'){
          fore<- dlmForecast(data, nAhead = input$statePeriod, sampleNew=100)
          ciTheory20 <- (outer(sapply(fore$Q, FUN=function(x) sqrt(diag(x))), qnorm(c(0.2,0.8))) +
                         as.vector(t(fore$f)))
          ciTheory5 <- (outer(sapply(fore$Q, FUN=function(x) sqrt(diag(x))), qnorm(c(0.05,0.95))) +
                           as.vector(t(fore$f)))
          Forecast<-cbind(fore$f[,1], ciTheory20, ciTheory5)
          Forecast

        } else {
          
        }
      })
      
      stateForecast<- reactive({
        
        
        if ( input$StateModel=="Structural"){
          data<-state()
           f<-forecast(data, h= input$statePeriod)
        } else if (input$StateModel=="dlm"){
          #model<- dlm()
          model<- filterDlm()
          fore<- dlmForecast(model, nAhead = input$statePeriod, sampleNew=100)
          ciTheory20 <- (outer(sapply(fore$Q, FUN=function(x) sqrt(diag(x))), qnorm(c(0.2,0.8))) +
                           as.vector(t(fore$f)))
          ciTheory5 <- (outer(sapply(fore$Q, FUN=function(x) sqrt(diag(x))), qnorm(c(0.05,0.95))) +
                          as.vector(t(fore$f)))
          f<-data.frame(fore$f[,1], ciTheory20, ciTheory5)
          
        } else {
          model<- marss()
          data<- MARSSHandler()[1,]
          f<-MARSSsimulate(model, tSteps = input$statePeriod +length(data), nsim = 1, silent = TRUE, miss.loc = NULL)
        }
        f
        
      })
      
      
      output$stateForecast<- DT::renderDataTable({
        
        if (input$StateModel=="Structural"){
          table<- data.frame(stateForecast())
          colnames(table)<-c("Forecast", "Low 80", "High 80", "Low 95", "High 95") 
        } else if (input$StateModel=="Autoregressive"){
          f<-stateForecast()
          forec<-data.frame(f$sim.data)
          forec2<-data.frame(f$sim.states)
          a<-data.frame(t(forec))
          b<-data.frame(t(forec2))
          table<- data.frame(a,b)
          colnames(table)<-c(rep("Forecast obs", length(input$paramsMARSS)), rep("State obs", length(input$paramsMARSS)))
          
        } else {
          data<-stateForecast()
          table<-data.frame(data)
          colnames(table)<-c("Forecast", "Low 80", "High 80", "Low 95", "High 95") 
        }
        DT::datatable(table)
        
      })
      
      
      sortDlmForecast<- function(x){
        
      }
      
      output$filter<-renderPrint({
        print(filterDlm())
      })
      
      dlmPlot<- reactive({
        
        data1<- data()
        data<-data1[, input$paramsDlm]
        filtered<-filterDlm()
        smooth<-smoothDlm()
        if (input$typeDlm=="Manual"){
          if ( input$seasType=='No seasonality' && input$dlmPolyOrder==1){
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
        #ggplot(data, aes(data[1,], data[2,]))
        plot(data, type="o", pch=19, bg="black")
        lines(filt ,lty = "dashed", lwd = 2, col="red")
        lines(smoothed,lty = "dotted", lwd = 2, col="blue")
        legend("bottomright", legend = c("Data", "Filtered","Smoothed"), lty = c(1,2,3), pch=c(19, NA, NA), col=c("black", "red", "blue"), lwd=c(1,2,2))
        
      })
      
      structuralPlot<- reactive({
        data<-state()
        data1<- data()
        data1<-data1[, input$paramsState]
        
        plot(data1, type="o", pch=19, bg="black")
        lines(fitted(data)[,1],lty = "dashed", lwd = 2, col="red")
        lines(tsSmooth(data)[,1],lty = "dotted", lwd = 2, col="blue")
        legend("bottomright", legend = c("Data", "Filtered","Smoothed"), lty = c(1,2,3), pch=c(19, NA, NA), col=c("black", "red", "blue"), lwd=c(1,2,2))
        
      })
      
      marssPlot<-reactive({
        
        data<-marss()
        data1<- MARSSHandler()
        
        plot(data1[1,], type="o", pch=19, bg="black")
        lines(marssKalman()[1,],lty = "dashed", lwd = 2, col="red")
        lines(marssSmooth()[1,],lty = "dotted", lwd = 2, col="blue")
        
      })
      
      output$stateFittedPlot<- renderPlot({
        
        if (input$StateModel=="dlm"){
          
          dlmPlot()
          
         } else if (input$StateModel=="Structural"){
           
           structuralPlot()
           
         } else {
           marssPlot()
        }
        })
      
      
      #Function creating a data.frame in the form required by ggplot in dlmForecastPlot()
      
      stateForecastPlotDataHandler<- reactive({
        
        data1<- data()
        data<-data.frame(data1[, input$paramsDlm])
        colnames(data)<- c("data")
        data$x<- c(1:length(data$data))
        fore<-stateForecast()
        fore<-data.frame(fore)
        colnames(fore)<-c("mean","min80", "max80", "min95", "max95")
        m1<- length(data$x)+1
        m2<- length(data$x)+ length(fore$mean)
        fore$x<-c(m1:m2)
        f<-full_join(data, fore, by = "x")
        f
        
      })
      
      
      #ggplot functionality like in arime.plot()
      
      dlmForecastPlot<- reactive({
        
        f<- stateForecastPlotDataHandler()
        g<- ggplot(f, aes(x=x, y=data)) + geom_line() + geom_line(aes(x=x, y=mean), colour = "blue")
        g<- g + geom_ribbon(aes(x=x, ymin=min80, ymax=max80),  fill = "lightsteelblue4",  alpha=0.3)
        g<- g + geom_ribbon(aes(x=x, ymin=min95, ymax=max95),  fill = "grey50",  alpha=0.3)
        g
        
      })
      
      
      marssForecastPlot<-reactive({
        
        data<- MARSSHandler()[1,]
        f<-stateForecast()
        a<-data.frame(f$sim.data)
        forecast<-t(a)
        plot(forecast[,1], type = 'o', lwd = 2, pch = 16, ylab = "Data")
        lines(data,type = 'o')
        #abline(v = length(data)+0.5, lty = "dashed")
        
      })
      
    
      
      output$stateForecastPlot<-renderPlot({
        
        if (input$StateModel=="dlm"){
          
          dlmForecastPlot()

          
        } else if (input$StateModel=="Autoregressive"){
          
          marssForecastPlot()
          
        } else {
          plot(stateForecast()) 
        }
      })
    })
    
    
  })


