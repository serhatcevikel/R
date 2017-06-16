library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
    faiz.geliri <- reactive((1000 *  (input$faiz / 100 * input$ay / 12)))

    output$textt <- renderText(paste("Faiz Geliri: ", round(faiz.geliri(), 2), " TL", sep = ""))

  output$pointplot <- renderPlot({
    plot(x = input$faiz,
         y = faiz.geliri(),
        xlim = c(0, 50),
        xlab = "Yıllık Faiz Oranı",
        ylim = c(0, 500),
        ylab = "Faiz Geliri",
         col = "red",
         bg = "red",
         pch = 16,
         cex = 2)
  })


})
