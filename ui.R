library(shiny)

# Define UI for slider demo application
shinyUI(fluidPage(

  #  Application title
  verticalLayout(
    titlePanel("Why standard deviation matters"),
    plotOutput("plotLines"),
    wellPanel(
      sliderInput("sd", "Standard Deviation:",
                  min = 5, max = 50, value = 10, step= 5)#

    ),
    wellPanel(
      p("Standard deviation regulates how widely future results could vary. The higher the standard deviation, the greater results may vary."),
      br(),
      p("In addition to regulating how widely results might vary, standard deviation also has a negative effect on cumulative returns. The higher the standard deviation, the worse returns are in general, which you can see by noticing how the projected 67th, 50th, 33rd, and 5th percentile values change with higher levels of standard deviation."),
      br(),
      p("The chart above assumes $10,000 initial wealth and an expected return of 6%. It is not a prediction, a forecast, or investment advice. It is merely an illustration of standard deviation's effect on wealth generation.")


    )
  )
))