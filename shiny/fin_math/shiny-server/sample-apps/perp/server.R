library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
    bugunku.deger <- reactive((1 + (input$g / 100)) / ((input$basit.faiz / 100) - (input$g / 100)))

    output$textt <- renderText(paste("Sonsuza kadar büyüyen 1 TL'nin Bugünkü Değeri: ", round(bugunku.deger(), 2), sep = ""))

  output$pointplot <- renderPlot({
    plot(x = input$basit.faiz,
         y = bugunku.deger(),
        xlim = c(6, 20),
        xlab = "Yıllık Faiz Oranı",
        ylim = c(1, 100),
        ylab = "Bugünkü Değer",
         col = "red",
         bg = "red",
         pch = 16,
         cex = 2)
  })


})
