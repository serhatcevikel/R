library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Bileşik Faiz + 1= [1 + (Basit Faiz * Gün / 365)] ^ (365 / Gün)"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("basit.faiz",
                  "Yıllık Basit Faiz Oranı (%):",
                  min = 0,
                  max = 50,
                  value = 40,
                  width = 600),
  
    sliderInput("gun",
                  "Gün Sayısı:",
                  min = 1,
                  max = 365,
                  value = 37,
                  width = 400),
    width = 4
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("pointplot", height=400, width = 400),
    span(textOutput("textt"), style="color: red; font-size: 30px; font-style: bold")
    )
  
))
