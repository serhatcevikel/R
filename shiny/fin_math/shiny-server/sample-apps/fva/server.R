library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
    gelecek.deger <- reactive(((1 + input$basit.faiz / 100)^input$yil - 1) / input$basit.faiz * 100)

    output$textt <- renderText(paste("n sene boyunca 1 TL'nin Gelecek Değeri: ", round(gelecek.deger(), 2), sep = ""))

  output$pointplot <- renderPlot({
    plot(x = input$basit.faiz,
         y = gelecek.deger(),
        xlim = c(1, 10),
        xlab = "Yıllık Faiz Oranı",
        ylim = c(0.8, 1200),
        ylab = "Gelecek Değer",
         col = "red",
         bg = "red",
         pch = 16,
         cex = 2,
         log = "y")
  })


})
