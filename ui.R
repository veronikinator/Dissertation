
# Define UI for application that draws a histogram
shinyUI(fluidPage(
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
      
      conditionalPanel(
        "output.fileUploaded",
        selectInput('params', 'Parameters:',
                           choices = NULL)
      )),
    mainPanel(
      uiOutput('summary'),
      tableOutput('forecast'),
      plotOutput("arimaplot")
    )
  )
))