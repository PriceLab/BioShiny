library(shiny)

ui <- fluidPage(
    titlePanel("dumb.R")
    )

server <- function(input, output, session) {
  x <- reactive(input$a * input$b)
  }

dumbApp <- shinyApp(ui, server)

testServer(dumbApp, {
  session$setInputs(a = 2, b = 3)
  stopifnot(x() == 6)
  printf("testServer: success")
  })
