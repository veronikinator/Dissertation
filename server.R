

# Define server logic required to draw a histogram
shinyServer(
  
  function(input, output, session) {
    
    
    #________________Getting all the uploaded data_______
    data<-reactive({
      inFile<- input$data
      req(inFile)
      table<-read.csv(inFile$datapath)
      vars<-names(table)
      updateSelectInput(session, "params","Select Columns", choices = vars)
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
    output$summary<-renderPrint({
      inFile<- data()
      req(inFile)
      arima()
    })
    
    output$forecast<- renderTable({
      inFile<- data()
      req(inFile)
      data.frame(forecast(arima()))
    })
    
    output$arimaplot<-renderPlot({
      inFile<- data()
      req(inFile)
      data1<-inFile[, input$params]
      arima<-auto.arima(data1)
     plot(arima)
      
      
    })

    
    
    #_____________Creating conditional panel_________

    
    output$fileUploaded <- reactive({
      return(!is.null(data()))
    })
    
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  })
