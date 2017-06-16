library(shiny)

# Define server logic required to draw a histogram
shinyServerx <- function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot

    real.int <- reactive(((1 + input$nominal/100) / (1 + input$enflasyon/100) -1) * 100)

    #output$real <- renderText(real.int())

  output$pointplot <- renderPlot({
    plot(y = real.int(),
         x = input$nominal,
        ylim = c(-100, 100),
        xlim = c(-100, 100),
         col = "red",
         bg = "red",
         pch = 21)
  })

}
