

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
      updateCheckboxGroupInput(session, inputId="paramsArimax", choices = vars, selected=vars[1])
      updateCheckboxGroupInput(session, inputId="paramsMARSS", choices = vars, selected=vars[1])
      table
    })
    
    #________________Outputting the uploaded table___________

    
    output$contents <- DT::renderDataTable({  
      DT::datatable(data())
    })


    #_______________Fitting the models_____
    
    arimaFit<-reactive({
      
      inFile<- data()

      if (input$arimaModel == "Manual"){
        params<-input$paramsArimax
        data1<-inFile[, params]
        x<-input$arimaOrder
        x <- as.numeric(c(unlist(strsplit(x, ","))))
        model<- arima(data1, order = x)
      } else {
        data1<-inFile[, input$paramsAutoArima]
        model<- auto.arima(data1)
      }
      
      model
    })
    
    state<-reactive({
      inFile<- data()
      data1<-inFile[, input$paramsState]
      type<-input$StateType
      StructTS(data1, type=type)
      
    })
    
    
    #______Construction forecast output_________
    
    observeEvent(input$analyseArima, {
      output$forecast<- DT::renderDataTable({
        inFile<- data()
        req(inFile)
        
          #table<- data.frame(MARSSsimulate(marss(), tSteps=input$period)$sim.data)
          #table<-t(table)
       
          table<- data.frame(forecast(fit(), h=input$period))
          colnames(table)<-c("Forecast", "Low 80", "High 80", "Low 95", "High 95")
          DT::datatable(table)
      })
      
      
      output$arimaplot<-renderPlot({
        inFile<- data()
        req(inFile)

          plot(arimaFit())
        
      })
      
      output$forecastplot<-renderPlot({
        inFile<- data()
        req(inFile)
       
          f<-forecast(fit(), h=input$period)
         plot(f)
        
      })
      
    })
    
    fit<- reactive({
      inFile<- data()
      req(inFile)
      fit<-arimaFit()            
      fit
    })
    
    
    #_____________Creating conditional panel_________

    
    output$fileUploaded <- reactive({
      return(!is.null(data()))
    })
    
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    
    #_______________Warnings handler_________________
    
    output$warn <- reactive({
      return(length(warning())>1)
    })
    
    outputOptions(output, 'warn', suspendWhenHidden=FALSE)
    
    #___________Console output_____________
    
    logText <- reactive({
      capture.output(arimaFit())
    })
    
    observeEvent(input$analyse, {
      output$console<- renderPrint({
        if (input$StateModel=='Autoregressive'){
          NULL
        } else{
          logText()
        }
        
      })
    
      output$warnings<- renderPrint({
        warning()
      })
    
      output$State<-renderPrint({
        "state"
      })
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
      output$MARSS<-renderPrint({
        mars<-marss()
        summary(mars$par)
        
      })
    })
    
    
  })


