library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
    payback.period <- reactive(apply(input$tbl, 1, payback))

    output$textt1 <- renderText(paste("Proje1 geri ödeme süresi: ", round(bugunku.deger(), 2), " yıldır" sep = ""))
    output$textt2 <- renderText(paste("Proje2 geri ödeme süresi: ", round(bugunku.deger(), 2), " yıldır" sep = ""))

  output$pointplot <- renderPlot({
    plot(x = input$f,
         y = bugunku.deger(),
        xlim = c(0, 5),
        xlab = "Proje 1 geri dönüş süresi",
        ylim = c(0, 5),
        ylab = "Proje 2 geri dönüş süresi",
         col = "red",
         bg = "red",
         pch = 16,
         cex = 2)
    abline(a = 0, b = 1)
  })


})

payback <- function(cfvec)
{
    cums <- cumsum(cfvec)
    partial <- cfvec[-1] / cums[-length(cums)] * (cums[-1] >= 0) * (cums[-length(cums)] < 0)
    payback <- length(cums[-1] < 0) + sum(partial)
    return(payback)
}




bono.fiyat <- function(k, n, f, t) {
    kup <- k / n
    faiz <- (1 + (f/100))^(1/n) - 1
    don <- n * t

    kuplar <- (kup / faiz) * (1 - (1 / ((1+faiz)^don)))

    anapara <- 100 / ((1 + faiz)^don)

    fiyat <- kuplar + anapara

    return(fiyat)

}
