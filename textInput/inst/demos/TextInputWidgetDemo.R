library(R6)
library(shiny)
library(TextInputWidget)
library(msgBoxWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

TextInputWidgetDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(textBox = NULL,
                   msgBox = NULL,
                   getTextButton = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            private$textBox = TextInputWidget$new(id="inputBox", title="Name",
                                                  initialValue="this is the inital value",
                                                  backgroundColor="white", fontSize=14,
                                                  boxWidth=400)
            private$msgBox = msgBoxWidget$new(id="msgBox", title="current text", boxWidth=400)
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              wellPanel(style="width: 1000px; height=150px;",
                actionButton("getTextButton", label= "Get Text", style="margin-top: 10px; margin-left: 20px;"),
                actionButton("setTextButton", label= "Set Text (broken)", style="margin-top: 10px; margin-left: 20px;")
                ),
              div(private$textBox$ui(), style="margin: 50px;"),
              div(private$msgBox$ui(),  style="margin: 50px;")
              )},

        #------------------------------------------------------------
        server = function(input, output, session){

            printf("entering TextInputWidgetDemo::server")
            private$textBox$server(input, output, session)
            private$msgBox$server(input, output, session)

            observeEvent(input$getTextButton, ignoreInit=TRUE, {
               result <- private$textBox$getText()
               printf("getTextButton, result: %s", result)
               private$msgBox$setText(result)
               })

            observeEvent(input$setTextButton, ignoreInit=TRUE, {
               private$textBox$setText("foo")
               })

            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- TextInputWidgetDemoApp$new()
x <- shinyApp(app$ui, app$server)
runApp(x, port=1115)

