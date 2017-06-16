library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
    bugunku.deger <- reactive(bono.fiyat(input$k, input$n, input$f, input$t))

    output$textt <- renderText(paste("Tahvil Fiyatı: ", round(bugunku.deger(), 2), sep = ""))

  output$pointplot <- renderPlot({
    plot(x = input$f,
         y = bugunku.deger(),
        xlim = c(0, 30),
        xlab = "Yıllık Bileşik Faiz Oranı",
        ylim = c(0, 400),
        ylab = "Tahvil Fiyatı",
         col = "red",
         bg = "red",
         pch = 16,
         cex = 2)
  })


})


bono.fiyat <- function(k, n, f, t) {
    kup <- k / n
    faiz <- (1 + (f/100))^(1/n) - 1
    don <- n * t

    kuplar <- (kup / faiz) * (1 - (1 / ((1+faiz)^don)))

    anapara <- 100 / ((1 + faiz)^don)

    fiyat <- kuplar + anapara

    return(fiyat)

}
