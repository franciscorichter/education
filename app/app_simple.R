# Simple test version
library(shiny)

ui <- fluidPage(
  titlePanel("ENLA Educational Data Analysis Platform - Test Version"),
  h3("Status: App is working!"),
  p("The app has been successfully loaded with improved error handling.")
)

server <- function(input, output) {
  # Empty server function for testing
}

shinyApp(ui, server)
