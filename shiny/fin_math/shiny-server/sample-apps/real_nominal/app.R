library(shiny)

# Define UI for application that plots random distributions 
shinyUIx <- pageWithSidebar(
  
  # Application title
  headerPanel("(1 + Nom. Faiz) / (1 + Enf.) = (1 + Reel Faiz)"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("nominal",
                  "Nominal Faiz Oranı (%):",
                  min = 0,
                  max = 50,
                  value = 20),
  
    sliderInput("enflasyon",
                  "Enflasyon Oranı Oranı (%):",
                  min = 0,
                  max = 50,
                  value = 20)
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("pointplot", height=400, width = 400),
    #span(textOutput("real"), style="color: red; font-size: 20px; font-style: italic")
    span(textOutput("real"), style="color: red; font-size: 30px; font-style: bold")
        )
)


# Define server logic required to draw a histogram
shinyServerx <- function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #  2) Its output type is a plot

    real.int <- reactive((((1 + input$nominal/100) / (1 + input$enflasyon/100)) -1) * 100)

    output$real <- renderText(paste("Reel Faiz Oranı: ", real.int(), sep = ""))

  output$pointplot <- renderPlot({
    plot(y = real.int(),
         x = input$nominal,
        ylim = c(-50, 50),
        ylab = "Reel Faiz Oranı",
        xlim = c(0, 50),
        xlab = "Nominal Faiz Oranı",
         col = "red",
         bg = "red",
         pch = 21)
  })

}


shinyApp(ui = shinyUIx, server = shinyServerx)         
