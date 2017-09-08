library(shiny)
library(shinyIncubator)

projeler <- rbind(Proje1 = c(-1000, 120, 380, -50, 350, 520, 100, 220),
                  Proje2 = c(-1200, 440, 330, 180, 20, 400, 150, 120)
                  )

colnames(projeler) <- 0:5


# Define UI for application that plots random distributions 
ui <- pageWithSidebar(
  # Application title
  headerPanel("İki projenin nakit akışlarının farklı yöntemlerle değerlendirilmesi"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    matrixInput("tbl", "Proje Nakit Akışlarını Giriniz:", as.data.frame(projeler))) ,

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("pointplot", height=400, width = 400),
    span(textOutput("textt1"), style="color: red; font-size: 30px; font-style: bold"),
    span(textOutput("textt2"), style="color: red; font-size: 30px; font-style: bold")
    )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot
    payback.period <- reactive(apply(input$tbl, 1, payback))

    output$textt1 <- renderText(paste("Proje1 geri ödeme süresi: ", round(payback.period()[1], 2), " yıldır", sep = ""))
    output$textt2 <- renderText(paste("Proje2 geri ödeme süresi: ", round(payback.period()[2], 2), " yıldır", sep = ""))

  output$pointplot <- renderPlot({
    plot(x = payback.period()[1],
         y = payback.period()[2],
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


}

payback <- function(cfvec)
{
    cums <- cumsum(cfvec)
    partial <- - cums[-length(cums)] / cfvec[-1] * (cums[-1] >= 0) * (cums[-length(cums)] < 0)
    payback <- sum(cums[-1] < 0) + sum(partial)
    return(payback)
}


shinyApp(ui = ui, server = server)
