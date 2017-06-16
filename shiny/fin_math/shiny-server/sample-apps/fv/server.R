library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
    gelecek.deger <- reactive(1000 * ((1 + input$basit.faiz / 100)^input$yil))

    output$textt <- renderText(paste("1000 TL'nin Gelecek Değeri: ", round(gelecek.deger(), 0), sep = ""))

  output$pointplot <- renderPlot({
    plot(x = input$basit.faiz,
         y = gelecek.deger(),
        xlim = c(0, 10),
        xlab = "Yıllık Faiz Oranı",
        ylim = c(1000, 120000),
        ylab = "Gelecek Değer",
         col = "red",
         bg = "red",
         pch = 16,
         cex = 2,
         log = "y")
  })


})
