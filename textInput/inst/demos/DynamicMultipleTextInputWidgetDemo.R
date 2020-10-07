library(R6)
library(shiny)
library(TextInputWidget)
library(msgBoxWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

DemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(divID = NULL,
                   widgetList = NULL,
                   widgetCount = NULL,
                   input = NULL,
                   output = NULL,
                   session = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(divID){
            private$divID = divID
            private$widgetList = list()
            private$widgetCount = 0;
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              wellPanel(style="width: 1000px; height=150px;",
                actionButton("newWidgetButton", label="+"),
                actionButton("saveButton", label="Save"),
                actionButton("getTextButton", label="Get All Text")
                ),
              div(id="multiDynamicDiv")
              )},

        #------------------------------------------------------------
        server = function(input, output, session){

            private$input <- input
            private$output <- output
            private$session <- session

            observeEvent(input$newWidgetButton, ignoreInit=TRUE, {
               private$widgetCount <- private$widgetCount + 1;

               widgetID <- sprintf("textInputBox.%02d", private$widgetCount)
               printf("widgetID: '%s'", widgetID)
               tiw <- TextInputWidget$new(id=widgetID, title="",
                                          initialValue=widgetID,
                                          backgroundColor="white", fontSize=14,
                                          boxWidth=400)
               private$widgetList[[private$widgetCount]] <- tiw
               insertUI(selector="#multiDynamicDiv", where="afterEnd", tiw$ui(), immediate=TRUE)
               tiw$server(private$input, private$output, private$session)
               printf("new widget requested")
               })

            observeEvent(input$saveButton, ignoreInit=TRUE, {
               printf("save text requested")
               })

            observeEvent(input$getTextButton, ignoreInit=TRUE, {
               printf("widget count: %d", private$widgetCount)
               print(private$widgetList[[1]]$getText())
               x <- as.character(lapply(private$widgetList, function(widget) widget$getText()))
               print(x)
               })

            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- DemoApp$new("test")
x <- shinyApp(app$ui, app$server)
runApp(x, port=1116)

