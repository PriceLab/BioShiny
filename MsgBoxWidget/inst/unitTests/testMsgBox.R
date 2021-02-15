library(R6)
library(shiny)
library(MsgBoxWidget)
library(RUnit)
library(later)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

MsgBoxDemoApp = R6Class("MsgBoxApp",

    #--------------------------------------------------------------------------------
    private = list(msgBox1 = NULL,
                   msgBox2 = NULL,
                   randomTextButton = NULL),

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
              })
            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
#box = msgBoxWidget$new(id="box1", title="random mixed text")
r6.demo = MsgBoxDemoApp$new()
app <- shinyApp(r6.demo$ui, r6.demo$server)

testServer(app, {
   printf("--- entering testServer")
   session$setInputs(randomTextButton=1)
   browser()
   print(currentText())
   })


