# https://groups.google.com/forum/#!topic/shiny-discuss/DK32wBrReHM

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
        numericInput("nrow", "Number of Rows", 2,1,10),
        numericInput("ncol", "Number of Columns", 2,2,10),
        uiOutput("inmatrix")
    ),
        mainPanel(
        tableOutput("showtable")
    )
))

shinyApp(ui = ui, server = server)



