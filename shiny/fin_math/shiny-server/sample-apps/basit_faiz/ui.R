library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Dönem Faizi = Anapara * (Yıllık Faiz * Ay Sayısı / 12)"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("faiz",
                  "Yıllık Faiz Oranı (%):",
                  min = 0,
                  max = 50,
                  value = 50,
                  width = 600),
  
    sliderInput("ay",
                  "Mevduat Vadesi (ay):",
                  min = 0,
                  max = 12,
                  value = 6,
                  width = 400),
    width = 4
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("pointplot", height=400, width = 400),
    #span(textOutput("real"), style="color: red; font-size: 20px; font-style: italic")
    span(textOutput("textt"), style="color: red; font-size: 30px; font-style: bold")
    )
  
))
