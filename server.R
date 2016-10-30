

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
    
    parameters<- reactive ({
      inFile<- input$data
      req(inFile)
      vars <- names(data())
      vars
    })
    
    #________________Outputting the uploaded table___________

    
    output$contents <- DT::renderDataTable({  
      DT::datatable(data())
    })
    
   
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
    
    
    #______Construction forecast output_________
    
    output$forecast<- renderTable({
      inFile<- data()
      req(inFile)
      fit<- switch(input$model, 
                      "Auto Arima"={
                        arima()
                      },
                      "State"={
                        state()
                      })
      table<-data.frame(forecast(fit))
      colnames(table)<-c("Forecast", "Low 80", "High 80", "Low 95", "High 95")
      table
    })
    
    output$arimaplot<-renderPlot({
      inFile<- data()
      req(inFile)
      data1<-inFile[, input$params]
      arima<-auto.arima(data1)
     plot(arima)
      
      
    })
    
    output$forecastplot<-renderPlot({
      inFile<- data()
      req(inFile)
      fitted<- switch(input$model,
                      "Auto Arima"= {
                        arima()
                      })
      plot(forecast(fitted))
      
      
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


