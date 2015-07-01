source("StandardDeviation_CumulativeGrowth_sandbox.R")

library(shiny)

# Define server logic for slider examples
shinyServer(function(input, output) {
  output$plotLines <- renderPlot({genPlot(input$sd, input$years, input$return)})
})