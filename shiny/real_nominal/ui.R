library(shiny)

# Define UI for application that plots random distributions 
shinyUI <- pageWithSidebar(
  
  # Application title
  headerPanel("(1 + Nom. Faiz) / (1 + Enf.) = (1 + Reel Faiz)"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("nominal",
                  "Nominal Faiz Oranı (%):",
                  min = 1,
                  max = 50,
                  value = 20),
  
    sliderInput("enflasyon",
                  "Enflasyon Oranı Oranı (%):",
                  min = 1,
                  max = 50,
                  value = 20)
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("pointplot", height=250)
    #textOutput("real")
  )
)
