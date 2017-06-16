library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("k kupon oranlı, yılda n kere ödeme yapan, f faiz oranlı, t vadeli ve anaparası 100 TL olan tahvilin bugünkü değeri = \r (anapara * k / n) / ((1 + f)^(1/n)-1) * [1 - (1 / (1 + ((1 + f)^(1/n)-1))^(t*n))] \r + (anapara / ((1 + ((1 + f)^(1/n)-1))^(t*n)))"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("f",
                  "Yıllık Bileşik Faiz Oranı (%):",
                  min = 1,
                  max = 30,
                  value = 26.56,
                  width = 600),
  
    sliderInput("k",
                  "Yıllık Kupon Oranı (%):",
                  min = 0,
                  max = 30,
                  value = 25,
                  width = 600),
  
    sliderInput("n",
                  "Yıllık Kupon Adedi:",
                  min = 1,
                  max = 2,
                  value = 2,
                  width = 600),
  
    sliderInput("t",
                  "Vadeye Kalan Yıl Sayısı:",
                  min = 1,
                  max = 10,
                  value = 2,
                  width = 400),
    width = 4
  ),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("pointplot", height=400, width = 400),
    span(textOutput("textt"), style="color: red; font-size: 30px; font-style: bold")
    )
  
)
)
