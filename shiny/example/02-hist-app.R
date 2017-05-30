library(shiny)
library(stringr)

ui <- fluidPage(

#textInput(inputId, label, value = "", width = NULL, placeholder = NULL)
  textInput(inputId = "text", 
    #label = "Input some text", 
    label ="İngilizce ifadeyi yazın", 
    value = ""),
  textOutput("reverse")
)

server <- function(input, output) {
  #output$reverse <- renderText((revstring(input$text)))
  output$reverse <- renderText((trans.en.tr(input$text)))
}


revstring <- function(textt) {
    paste(rev(unlist(strsplit(textt, ""))), collapse = "")
}

trans.en.tr <- function(textt) {
    aa <- system(sprintf('trans en:tr "%s"', textt), intern = T)
    aa <- aa[length(aa)]
    aa <- stringr::str_replace_all(aa, "\033.[0-9]{1,2}m", "")
    return(aa)
}



shinyApp(ui = ui, server = server)
