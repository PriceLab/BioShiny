library(R6)
library(shiny)
library(dataTableWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

DataTableDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(dataTable1 = NULL,
                   randomTextButton = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            private$dataTable1 = dataTableWidget$new(id="tbl.1", tbl=head(mtcars))
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              wellPanel(style="width: 1000px;",
                actionButton("randomTextButton", label="Generate random Text",
                             style="margin-top: 40px; margin-left: 200px;")),
                private$dataTable1$ui()
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            printf("entering dataTableWidgetDemo::server")
            private$dataTable1$server(input, output, session)
            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- DataTableDemoApp$new()
x <- shinyApp(app$ui, app$server)
runApp(x, port=1112)

