library(shiny)
library(shinyIncubator)

projeler <- rbind(Proje1 = c(-1000, 120, 380, -50, 350, 520),
                  Proje2 = c(-1200, 440, 330, 180, 20, 400)
                  )

colnames(projeler) <- 0:5


# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("İki projenin nakit akışlarının farklı yöntemlerle değerlendirilmesi"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    matrixInput("tbl", "Proje Nakit Akışlarını Giriniz:", projeler)) ,

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("pointplot", height=400, width = 400),
    span(textOutput("textt1"), style="color: red; font-size: 30px; font-style: bold"),
    span(textOutput("textt2"), style="color: red; font-size: 30px; font-style: bold")
    )
  
)
)
