

# Define server logic required to draw a histogram
shinyServer(
  
  function(input, output, session) {
  
    #________________Getting all the uploaded data_______
    
    makeList<- function(data){
      l<-list()
      for ( i in 1:length(data)){
        l[[toString(data[i])]]<-data[i]
      }
      return(l)
    }
    
    data<-reactive({
      inFile<- input$data
      req(inFile)
      table<-read.csv(inFile$datapath)
      vars<-names(table)
      vars1<-makeList(vars)
      updateSelectInput(session, "paramsAutoArima","Select Columns:", choices = vars)
      updateSelectInput(session, "paramsState","Select Columns:", choices = vars)
      updateSelectInput(session, "paramsDlm","Select Columns:", choices = vars)
      updateSelectInput(session, "paramsArima", "Select data:", choices = vars)
      updateCheckboxGroupInput(session, inputId="xregParamsArimax", choices = vars, selected=NULL)
      updateCheckboxGroupInput(session, inputId="paramsMARSS", choices = vars, selected=vars[1])
      table
    })
    
    #________________Outputting the uploaded table___________

    
    output$contents <- DT::renderDataTable({  
      DT::datatable(data())
    })


    #_______________Arima_____
    
    
    xreg<- reactive({
      input$xregParamsArimax
    })

    
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
    
    arimaForecast<- reactive({
      if (is.null(xreg())){
        
        f<- forecast(arimaFit(), h=input$arimaPeriod)
      } else{
        f<-forecast(arimaFit(), h=input$arimaPeriod, xreg=data()[,xreg()])
      }
      
    })
    
    logText <- reactive({
      capture.output(arimaFit())
    })
    
    
    #______Construction forecast output_________
    
    observeEvent(input$analyseArima, {
      
      output$console<- renderPrint({
        
        logText()
        
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
  
  
  buildModReg<-function(v){
    ##########################
    ###TODO: 4 parameters input handler
    ############################
    dV<- exp(v[1])
    dW<- c(exp(v[2]), 0)
    m0<- v[3:4]
    dlmModReg(x, dV= dV, dW=dW, m0=m0)
    
  }  
  
  
  initGuessParams<- reactive({
    
    inFile<- data()
    data<-inFile[, input$paramsDlm]
    varGuess<- var(diff(data), na.rm=TRUE)
    mu0Guess<-data[1]
    lambdaGuess<-mean(diff(data), na.rm=TRUE)
    params<- c(log(varGuess), log(varGuess/5), mu0Guess, lambdaGuess)
    mle<- dlmMLE(data, parm = params, build = buildModReg,  method = "Nelder-Mead")
    mle
    
  })
  
  dlm<-reactive({
    
    x<-input$dlmParams
    x<-as.numeric(unlist(strsplit(x, ",")))
    
    init<-initGuessParams()
    if (x != c(0,0,0,0)){
      params<- x
    } else{
      params<- init$par
    }
    dlm<-buildModReg(params)
    dlm
    
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
        summary(model)
        
      })
      
      output$stateDiag<-renderPlot({
        
        tsdiag(modelState())
        
      })
      
      stateForecast<- reactive({
        
        data<-state()
        forecast(data, h= input$statePeriod)
        
      })
      
      
      output$stateForecast<- DT::renderDataTable({
        
        table<- data.frame(stateForecast())
        colnames(table)<-c("Forecast", "Low 80", "High 80", "Low 95", "High 95")
        DT::datatable(table)
        
      })
      
      output$stateFittedPlot<- renderPlot({
        
        data<-state()
        TimeSeries <- window(data$data, start = 0)
        plot(TimeSeries, type = "o")
        lines(fitted(data), lty = "dashed", lwd = 2)
        lines(tsSmooth(data), lty = "dotted", lwd = 2)
        legend("bottomright", legend = c("Data","Smoothed", "Filtered"),lty = c(1,1,1), col=c("black","blue", "red"))
        
        })
      
      output$stateForecastPlot<-renderPlot({
        
        plot(stateForecast())
      })
    })
    
    
  })


