# https://stackoverflow.com/questions/33603517/change-dataframe-values-when-user-inputs-numericinput-shiny

data <- as.data.frame(c(98,99,34))
names(data) <- "Projection"
data$User_Prediction <- 0

ui <- shinyUI(
                fluidPage(
                              titlePanel("Basic DataTable"),



                                  # Create a new row for the table.
                                  fluidRow(
                                                 column(12,
                                                                     selectInput("select", label = h3("Select box"), 
                                                                                                          choices = unique(data$Projection), 
                                                                                                                                   selected = unique(data$Projection)[1]),
                                                                     numericInput("num", label = h3("Numeric input"), value = unique(data$Projection)[1]),
                                                                                  actionButton('btn',"Apply Changes"),
                                                                                  dataTableOutput(outputId="table")
                                                                                               )
                                                     )    
                                )  
                )

server <- shinyServer(function(input, output) {
                            d <- reactive({
                                    data
                                      })

                              dat <- reactiveValues(dat=NULL)
                              observe({
                                      dat$dat <- d()
                                        })

                                observe({
                                        input$btn
                                            isolate({
                                                      num <- input$num
                                                            sel <- input$select
                                                          })
                                            dat$dat$User_Prediction[dat$dat$Projection==sel] <- num
                                                #d2 <- dat
                                              })

                                # Better way
                              #   observeEvent(input$btn,{
                              #     dat$dat$User_Prediction[dat$dat$Projection==sel] <- num
                              #   })

                                # Filter data based on selections
                                output$table <- renderDataTable({
                                        dat$dat
                                          })
                })

shinyApp(ui=ui,server=server)
