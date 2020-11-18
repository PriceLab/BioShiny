library(shiny)
library(msgBoxWidget)
box <- msgBoxWidget$new(id="box1", title="foo")
boxApp <- shinyApp(box$ui, box$server)

testServer(boxApp, {
   printf("testServer")
   session$setInputs
   })



