library(R6)
library(shiny)
library(HeatmapWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

HeatmapDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(heatmap = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            mtx.file.1 <- "genesByScoredCandidateTFs.mtx.16x113.RData"
            mtx.file.2 <- "genesByScoredCandidateTFs.mtx.22x340.RData"
            mtx <- get(load(system.file(package="HeatmapWidget", "extdata", mtx.file.2)))
            private$heatmap = HeatmapWidget$new(id="box1", title="random mixed text", mtx)
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              wellPanel(style="width: 1000px;",
                actionButton("randomTextButton", label= "Generate random Text",
                             style="margin-top: 40px; margin-left: 200px;")),
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

