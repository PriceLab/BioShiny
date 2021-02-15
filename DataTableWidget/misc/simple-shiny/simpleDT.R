library(shiny)
library(DT)

ui <- fluidPage(

  # Application title
  titlePanel("Hello Shiny!"),

  sidebarLayout(
      sidebarPanel(
        sliderInput("obs", "Number of observations:", min = 0, max = 1000,value = 500)
        ),
    mainPanel(
      DT::dataTableOutput("dt")
    ),
     position = c("left", "right"),
     fluid =TRUE
    ) # sidebarLyout
  ) # ui

server <- function(input, output) {
   output$dt <- DT::renderDataTable({cbind(mtcars, mtcars)})
  }


runApp(shinyApp(ui, server), port=9998)


