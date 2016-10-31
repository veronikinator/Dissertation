

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
      table
    })
    
    #________________Outputting the uploaded table___________

    
    observeEvent(input$analyse, 
                 {output$contents <- DT::renderDataTable({  
      DT::datatable(data())
    })})
    

    #_______________Fitting the models_____
    
    arima<-reactive({
      inFile<- data()
      data1<-inFile[, input$paramsAutoArima]
      auto.arima(data1)
    })
    
    state<-reactive({
      inFile<- data()
      data1<-inFile[, input$paramsState]
      type<-input$StateType
      StructTS(data1, type=type)
      
    })
    
    marss<-reactive({
      inFile<- data()
      data1<-inFile[, input$paramsState]
      MARSS(c(data1))
    })
    
    
    #______Construction forecast output_________
    
    observeEvent(input$analyse, {
      output$forecast<- DT::renderDataTable({
        inFile<- data()
        req(inFile)
      
        table<- data.frame(forecast(fit(), h=input$period))
        colnames(table)<-c("Forecast", "Low 80", "High 80", "Low 95", "High 95")
        DT::datatable(table)
      })
      output$arimaplot<-renderPlot({
        inFile<- data()
        req(inFile)
        plot(arima())
        
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
      fit<- switch(input$model, 
                   "Auto Arima"={
                     arima()
                   },
                   "State"={
                     marss()
                   })
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
      capture.output(arima())
    })
    
    observeEvent(input$analyse, {
      output$console<- renderPrint({
        logText()
      })
    
      output$warnings<- renderPrint({
        warning()
      })
    
      output$State<-renderPrint({
        "state"
      })
    })
  })


