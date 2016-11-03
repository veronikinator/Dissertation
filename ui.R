css<- "div.box {
      width: 250px;
      border: 2px solid rgb(191, 0, 55);
padding: 2px;
margin: 2px;
}"




# Define UI for application that draws a histogram
shinyUI(

  navbarPage("Time Series Analysis App",
    
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
        DT::dataTableOutput('contents')
      )
    ),
    tabPanel("Arima",
               sidebarPanel(
                 numericInput("period", "Choose a number of periods to forecast:", 10),
                 selectInput( "model", "Choose model:", choices = c("Auto Arima", "Manual")
                              ),
                 conditionalPanel("input.model=='Auto Arima'",
                                  selectInput('paramsAutoArima', 'Parameters:', choices = NULL)),
                 conditionalPanel("input.model=='Manual'",
                                  checkboxGroupInput("paramsArimax", "Choose columns", choices = NULL)),
                 actionButton("analyseArima", label = "Analyse"),
                 conditionalPanel("input.analyseArima > 0",
                                  tags$br(),
                                  tags$div(class="box", textOutput("console")))
                 ),
             mainPanel(
               tags$h4("Forecast Table"),
               DT::dataTableOutput('forecast'),
               plotOutput("forecastplot"),
               conditionalPanel("input.model=='Auto Arima'",
                         plotOutput("arimaplot"))
               )
      ),
    tabPanel("Space-State",
               sidebarPanel(
                 selectInput("StateModel", "Choose model:", choices=c("Structural", "Autoregressive")),
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
               textOutput("MARSS")
               )
    )
))

# 
# shinyUI(
#   navbarPage("Page Title",
#              
#              tabPanel("Panel 1",
#                       sidebarPanel(
#                                   fileInput('data', 'Choose file to upload', accept = c('text/csv',
#                                                                                       'text/comma-separated-values',
#                                                                                       'text/tab-separated-values',
#                                                                                       'text/plain',
#                                                                                       '.csv',
#                                                                                       '.tsv')
#                                             )
#                                 )     
#                       
#                       
#                       ),
#              tabPanel("Panel 2"),
#              tabPanel("Panel 3")
#   )
# )