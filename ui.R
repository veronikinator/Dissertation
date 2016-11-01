

css<- "div.box {
      width: 250px;
      border: 2px solid rgb(191, 0, 55);
      padding: 2px;
      margin: 2px;
    }"




# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(
    tags$style(type="text/css", css)),
    
  titlePanel("Time Series Analysis App"),
  sidebarLayout(
    sidebarPanel(
      fileInput('data', 'Choose file to upload', accept = c('text/csv',
                                                            'text/comma-separated-values',
                                                            'text/tab-separated-values',
                                                            'text/plain',
                                                            '.csv',
                                                            '.tsv')
                ),
      tags$hr(),
      #matrixInput('foo', 'Foo', head()),
      
      
      conditionalPanel( "output.fileUploaded",
                        
        selectInput( 
          "model", "Choose model:", choices = c("Auto Arima", "Arimax", "ETS", "State")
        ),
        
        numericInput("period", "Choose a number of periods to forecast:", 10),
        
        conditionalPanel("input.model=='Auto Arima'",
                         selectInput('paramsAutoArima', 'Parameters:', choices = NULL)),
        
        conditionalPanel("input.model=='Arimax'",
                         checkboxGroupInput("paramsArimax", "Choose columns", choices = NULL)),
        
        conditionalPanel("input.model=='State'",
                         
                         selectInput("StateModel", "Choose model:", choices=c("Structural", "Autoregressive")),
                         
                         conditionalPanel("input.StateModel=='Structural'",
                                          selectInput("paramsState", "Choose columns", choices = NULL),
                                          
                                          selectInput("StateType", "Choose type:", choices=c("level", "trend", "BSM"), selected="level")
                                          ),
                         
                         conditionalPanel("input.StateModel=='Autoregressive'",
                                          checkboxGroupInput("paramsMARSS", "Choose columns", choices = NULL)
                                          )
                         ),
        
        actionButton("analyse", label = "Analyse"),
        
        conditionalPanel("input.analyse == 1",
                         
                         tags$br(),
                         tags$div(class="box", textOutput("console"))),
        
        conditionalPanel( "input.analyse == 1",
                       tags$div(class="box", 
                                 verbatimTextOutput("warnings"))))
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("Forecast",
                 
               tags$h4("Forecast Table"),
               
               DT::dataTableOutput('forecast'),
               
               plotOutput("forecastplot"),
               
               conditionalPanel("input.model=='State'",
                                textOutput("MARSS")
                                ),
               conditionalPanel("input.model=='Auto Arima'",
                                plotOutput("arimaplot"))
               ),
      tabPanel("Data",
               DT::dataTableOutput('contents')
               )
      )
    )
  )
))