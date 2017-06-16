library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
    bilesik.faiz <- reactive((((1+(input$basit.faiz * input$gun / 36500))^(365 / input$gun) -1) * 100))

    output$textt <- renderText(paste("Yıllık Bileşik Faiz Oranı (%): ", round(bilesik.faiz(), 2), sep = ""))

  output$pointplot <- renderPlot({
    plot(x = input$basit.faiz,
         y = bilesik.faiz(),
        xlim = c(0, 50),
        xlab = "Yıllık Basit Faiz Oranı",
        ylim = c(0, 65),
        ylab = "Yıllık Bilesik Faiz Oranı",
         col = "red",
         bg = "red",
         pch = 16,
         cex = 2)
  })


})
