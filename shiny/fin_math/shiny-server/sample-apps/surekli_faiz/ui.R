library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Sürekli Faiz ile Yılsonu Değer = Anapara (3000 TL) * e^(Yıllık Basit Faiz * Ay / 12)"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("basit.faiz",
                  "Yıllık Basit Faiz Oranı (%):",
                  min = 0,
                  max = 50,
                  value = 12,
                  width = 600),
  
    sliderInput("ay",
                  "Ay Sayısı:",
                  min = 1,
                  max = 12,
                  value = 9,
                  width = 400),
    width = 4
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("pointplot", height=400, width = 400),
    span(textOutput("textt"), style="color: red; font-size: 30px; font-style: bold")
    )
  
))
