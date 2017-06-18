
library(shiny)
library(shinyIncubator)

server <- function(input, output) {
    output$inmatrix <- renderUI({
            #if(is.null(input$tbl)) return(matrix(0,nrow=2,ncol=2))
            matrixInput("tbl", "Enter Data", as.data.frame(matrix(0,nrow=input$nrow,ncol=input$ncol)))
              })
  output$showtable <- renderTable({input$tbl})
}


ui <- shinyUI(pageWithSidebar(
        headerPanel('Simple matrixInput example'),
        sidebarPanel(
        matrixInput("tbl", "Enter Data", as.data.frame(matrix(c(-100, 5, 10, 20, 4, 40),nrow=1,ncol=6)))
    ),
        mainPanel(
        tableOutput("showtable")
    )
))

shinyApp(ui = ui, server = server)
