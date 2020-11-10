library(R6)
library(shiny)
library(HeatmapWidget)
library(shinyWidgets)
options(warn=2)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

HeatmapDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(heatmap = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            private$heatmap = HeatmapWidget$new(id="box1",
                                                title="mtcars",
                                                mtx=as.matrix(mtcars),
                                                rowTitle="model",
                                                columnTitle="attributes",
                                                width=1000,
                                                height=800)
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
                private$heatmap$ui(),
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            printf("entering heatmapWidgetDemo::server")
            private$heatmap$server(input, output, session)
            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- HeatmapDemoApp$new()
x <- shinyApp(app$ui, app$server)
runApp(x, port=1112)

