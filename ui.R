

css<- "div.box {
      width: 250px;
      border: 5px solid red;
      padding: 5px;
      margin: 5px;
    }"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(
    tags$style(type="text/css", css)),
    
  titlePanel("Time Series Analysis App"),
  sidebarLayout(
    sidebarPanel(
      fileInput('data', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      selectInput(
        "model", "Choose model:", choices = c("Auto Arima", "Arimax", "ETS")
        ),
      conditionalPanel(
        "output.fileUploaded",
        selectInput('params', 'Parameters:',
                           choices = NULL),
        tags$div(class="box", 
                 textOutput("console"))),
      conditionalPanel( "output.warn",
                       tags$div(class="box", 
                                 verbatimTextOutput("warnings")))
      ),
    mainPanel(
      tableOutput('forecast'),
      plotOutput("forecastplot"),
      plotOutput("arimaplot")
    )
  )
))