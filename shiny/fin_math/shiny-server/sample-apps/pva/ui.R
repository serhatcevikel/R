library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("n Sene Boyunca 1 TL'nin bugünkü değeri = 1 / Yıllık Faiz * [1 - 1 / (1 + Yıllık Faiz)^n]"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("basit.faiz",
                  "Yıllık Faiz Oranı (%):",
                  min = 0,
                  max = 20,
                  value = 10,
                  width = 600),
  
    sliderInput("yil",
                  "Yıl Sayısı:",
                  min = 1,
                  max = 50,
                  value = 5,
                  width = 400),
    width = 4
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("pointplot", height=400, width = 400),
    span(textOutput("textt"), style="color: red; font-size: 30px; font-style: bold")
    )
  
))
