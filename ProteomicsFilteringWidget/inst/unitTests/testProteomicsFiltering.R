library(R6)
library(shiny)
library(ProteomicsFilteringWidget)
library(RUnit)
library(later)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

PFApp = R6Class("PFApp",

    #--------------------------------------------------------------------------------
    private = list(msgBox1 = NULL,
                   msgBox2 = NULL,
                   randomTextButton = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            private$msgBox1 = ProteomicsFilteringWidget$new(id="box1", title="random mixed text")
            private$msgBox2 = ProteomicsFilteringWidget$new(id="box2", title="lower case", boxWidth=400, boxHeight=50,
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

            printf("entering ProteomicsFilteringWidgetDemo::server")
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
#box = ProteomicsFilteringWidget$new(id="box1", title="random mixed text")
demo = PFApp$new()
app <- shinyApp(demo$ui, demo$server)

testServer(app, {
   printf("--- entering testServer")
   session$setInputs(randomTextButton=1)
   browser()
   print(currentText())
   })


