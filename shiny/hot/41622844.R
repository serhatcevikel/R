# example from https://stackoverflow.com/questions/41622844/rhandsontable-and-shiny-glitches-while-editing-interactive-table

library(rhandsontable)
library(shiny)

ui = fluidPage(rHandsontableOutput("equation"))

server = function(input,output) {

      eqdf = data.frame(A_value = as.numeric(0),op = factor(c(">"),levels=c(">","<")),B_value = as.numeric(0))
  print(eqdf)
    values = reactiveValues(equation=eqdf)

    observe({
             req(input$equation)
                  values$equation = hot_to_r(input$equation)
               })

      output$equation = renderRHandsontable({
              rhandsontable(values$equation) 
                })
}

shinyApp(ui = ui,server = server)
