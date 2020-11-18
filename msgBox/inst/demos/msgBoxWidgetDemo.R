library(R6)
library(shiny)
library(msgBoxWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

MsgBoxDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(msgBox1 = NULL,
                   msgBox2 = NULL,
                   randomTextButton = NULL,
                   getTextButton = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            private$msgBox1 = msgBoxWidget$new(id="box1", title="random mixed text")
            private$msgBox2 = msgBoxWidget$new(id="box2", title="lower case", boxWidth=400, boxHeight=50,
                 fontSize=30, backgroundColor="lightgray")
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              wellPanel(style="width: 1000px;",
                actionButton("randomTextButton", label= "Generate random Text",
                             style="margin-top: 40px; margin-left: 200px;"),
                actionButton("getTextButton", label= "Get Text",
                             style="margin-top: 40px; margin-left: 200px;")),
                private$msgBox1$ui(),
                private$msgBox2$ui()
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            printf("entering msgBoxWidgetDemo::server")
            private$msgBox1$server(input, output, session)
            private$msgBox2$server(input, output, session)

            observeEvent(input$randomTextButton, ignoreInit=TRUE, {
              randomText <- paste(sample(c(LETTERS, letters), 10, replace=TRUE), collapse="")
              private$msgBox1$setText(randomText)
              private$msgBox2$setText(tolower(randomText))
              private$msgBox1$getText() == randomText
              private$msgBox2$getText() == tolower(randomText)
              })

            observeEvent(input$getTextButton, ignoreInit=TRUE, {
              print(private$msgBox1$getText())
              })

            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- MsgBoxDemoApp$new()
x <- shinyApp(app$ui, app$server)
runApp(x, port=1112)

