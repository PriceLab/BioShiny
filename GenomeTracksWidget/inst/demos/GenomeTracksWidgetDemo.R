library(R6)
library(shiny)
library(GenomeTracksWidget)
#----------------------------------------------------------------------------------------------------
addResourcePath("tracks", "tracks")
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

GenomeTracksWidgetDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(gtw = NULL,
                   tbl = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(tbl){
            printf("initializing demo")
            private$tbl <- tbl
            private$gtw <- GenomeTracksWidget$new("gtw1", "Genome Tracks")
            private$gtw$setTable(tbl)
            },

        #------------------------------------------------------------
        ui = function(){
            private$gtw$ui()
            },

        #------------------------------------------------------------
        server = function(input, output, session){

            printf("entering GenomeTracksWidgetDemo::server")
            private$gtw$server(input, output, session)

            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
filename <- "~/github/TrenaProjectErythropoiesis/inst/extdata/genomicRegions/tbl.3.0.250000.500000.RData"
#filename <- "../extdata/tbl-79.rows.RData"
#filename <- "../extdata/tbl-21.rows.RData"
#filename <- "../extdata/tbl-66.rows.RData"
tbl <- get(load(filename))


app <- GenomeTracksWidgetDemoApp$new(tbl)
x <- shinyApp(app$ui, app$server)
runApp(x, port=1118)

