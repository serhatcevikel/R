library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
    yilsonu.deger <- reactive(3000 * exp(input$basit.faiz * input$ay / 1200))

    output$textt <- renderText(paste("3000 TL'nin Yılsonu Değeri: ", round(yilsonu.deger(), 2), sep = ""))

  output$pointplot <- renderPlot({
    plot(x = input$basit.faiz,
         y = yilsonu.deger(),
        xlim = c(0, 50),
        xlab = "Yıllık Basit Faiz Oranı",
        ylim = c(3000, 5000),
        ylab = "Yılsonu Değeri",
         col = "red",
         bg = "red",
         pch = 16,
         cex = 2)
  })


})
