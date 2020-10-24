library(R6)
library(shiny)
library(tmsTableWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

TmsTableDemoDemo = R6Class("demo",

    #--------------------------------------------------------------------------------
    private = list(tmsTable = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(tbl){
            printf("initializing demo")
            private$tmsTable = tbl
            private$tmsTable = tmsTableWidget$new("tmstw1", "tmstw demo", tbl)
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              private$tmsTable$ui()
            )},

        #------------------------------------------------------------
        server = function(input, output, session){
            printf("entering tmsTableWidgetDemo::server")
            private$tmsTable$server(input, output, session)
            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
tbl <- get(load(system.file(package="tmsTableWidget", "extdata", "tbl.afp.srm.RData")))
demo <- TmsTableDemoDemo$new(tbl)
x <- shinyApp(demo$ui, demo$server)
runApp(x, port=1112)

