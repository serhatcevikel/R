library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("(1 + Nom. Faiz) / (1 + Enf.) = (1 + Reel Faiz)"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("nominal",
                  "Nominal Faiz Oranı (%):",
                  min = 0,
                  max = 50,
                  value = 20,
                  width = 600),
  
    sliderInput("enflasyon",
                  "Enflasyon Oranı Oranı (%):",
                  min = 0,
                  max = 50,
                  value = 20,
                  width = 400),
    width = 4
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("pointplot", height=400, width = 400),
    #span(textOutput("real"), style="color: red; font-size: 20px; font-style: italic")
    span(textOutput("real"), style="color: red; font-size: 30px; font-style: bold")
    )
  
))
