library(R6)
library(shiny)
library(dataTableWidget)
library(msgBoxWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

DataTableDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(dtWidget.1 = NULL,
                   randomTextButton = NULL,
                   tbl = NULL,
                   msgBox = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            private$msgBox =  msgBoxWidget$new(id="box1", title="table selection", boxWidth=600)
            private$tbl = mtcars
            private$dtWidget.1 = dataTableWidget$new(id="tbl.1", private$tbl, width=800, height=500)
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              wellPanel(style="width: 1000px;",
                private$msgBox$ui()),
                private$dtWidget.1$ui()
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            printf("entering dataTableWidgetDemo::server")
            private$msgBox$server(input, output, session)
            private$dtWidget.1$server(input, output, session)
            observe({
               row.names <- private$dtWidget.1$tableSelection()
               #row.names <- rownames(private$tbl)[row.numbers]
               #print(row.names)
               private$msgBox$setText(paste(row.names, collapse=", "))
               })
            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- DataTableDemoApp$new()
#x <- shinyApp(app$ui, app$server)a
runApp(shinyApp(app$ui, app$server), port=1112)

