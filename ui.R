library(shiny)

# Define UI for slider demo application
shinyUI(fluidPage(

  #  Application title
  titlePanel("Why your investment's standard deviation matters"),

  # Sidebar with sliders that demonstrate various available
  # options
  sidebarLayout(
    sidebarPanel(
      #sliderInput("return", "Expected Return:",
      #            min = 1, max = 8, value = 6, step= 1),

      sliderInput("sd", "Standard Deviation:",
                  min = 5, max = 50, value = 10, step= 5)#,

      #sliderInput("years", "Time horizon in years:",
       #           min = 10, max = 30, value = 30, step= 5)
    ),

    # Show a table summarizing the values entered
    mainPanel(
      plotOutput("plotLines")
    )
  )
))
