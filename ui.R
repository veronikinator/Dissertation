css<- "div.box {
      width: 250px;
      border: 2px solid rgb(191, 0, 55);
padding: 2px;
margin: 2px;
}"




# Define UI for application that draws a histogram
shinyUI(

  navbarPage("Time Series Analysis App", id = "tabs",
    
    tags$head(
      tags$style(type="text/css", css)
      ),
    
#     headerPanel("Time Series Analysis App"),
    
    tabPanel("Data",
        sidebarPanel(
          fileInput('data', 'Choose file to upload', accept = c('text/csv',
                                                              'text/comma-separated-values',
                                                              'text/tab-separated-values',
                                                              'text/plain',
                                                              '.csv',
                                                              '.tsv')
                    )
        ),
      mainPanel(
        DT::dataTableOutput('contents'),
        plotOutput("dataPlot")
      )
    ),
    tabPanel("Arima",
               sidebarPanel(
                 numericInput("arimaPeriod", "Choose a number of periods to forecast:", 10),
                 selectInput( "arimaModel", "Choose model:", choices = c("Auto Arima", "Manual")
                              ),
                 conditionalPanel("input.arimaModel=='Auto Arima'",
                                  selectInput('paramsAutoArima', 'Parameters:', choices = NULL)),
                 conditionalPanel("input.arimaModel=='Manual'",
                                  textInput("arimaOrder", "Choose the order of the model:", "0,0,0"),
                                  selectInput('paramsArima', 'Parameters:', choices = NULL),
                                  checkboxGroupInput("xregParamsArimax", "Choose explanatory variables:", choices = NULL)),
                 actionButton("analyseArima", label = "Analyse"),
                 conditionalPanel("input.analyseArima > 0",
                                  tags$br(),
                                  tags$div(class="box", textOutput("console")))
                 ),
             mainPanel(
               tags$h4("Forecast Table"),
               DT::dataTableOutput('arimaForecast'),
               plotOutput("arimaForecastPlot"),
               plotOutput("arimaPlot"),
               textOutput("arimaPrint")
               )
      ),
    tabPanel("Space-State",
               sidebarPanel(
                 selectInput("StateModel", "Choose model:", choices=c("Structural", "Autoregressive")),
                 numericInput("statePeriod", "Choose a number of periods to forecast:", 10),
                 conditionalPanel("input.StateModel=='Structural'",
                                  selectInput("paramsState", "Choose columns", choices = NULL),
                         
                                  selectInput("StateType", "Choose type:", choices=c("level", "trend", "BSM"), selected="level")
                         ),
                 conditionalPanel("input.StateModel=='Autoregressive'",
                                  checkboxGroupInput("paramsMARSS", "Choose columns", choices = NULL)
                                  ),
                 actionButton("analyseState", label = "Analyse")
                 ),
             mainPanel(
               textOutput("MARSS"),
               DT::dataTableOutput('stateForecast'),
               plotOutput("stateForecastPlot")
               )
    )
))
