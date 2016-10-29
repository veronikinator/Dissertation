

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
      updateSelectInput(session, "params","Select Columns", choices = vars)
      updateCheckboxGroupInput(session, inputId="inCheckbox", choices = vars)
      table
    })
    
    parameters<- reactive ({
      inFile<- input$data
      req(inFile)
      vars <- names(data())
      vars
    })
    
    #________________Outputting the uploaded table___________

    
    output$contents <- renderTable({  
      data()
    })
    
   
    #_______________Outputing auto.arima summary_____
    
    arima<-reactive({
      inFile<- data()
      data1<-inFile[, input$params]
      auto.arima(data1)
    })
    
    output$forecast<- renderTable({
      inFile<- data()
      req(inFile)
      table<-data.frame(forecast(arima()))
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
      data1<-inFile[, input$params]
      arima<-auto.arima(data1)
      plot(forecast(arima))
      
      
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
  })


