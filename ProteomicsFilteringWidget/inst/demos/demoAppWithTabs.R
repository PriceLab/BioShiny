#library(R6)
#library(shiny)
library(ProteomicsFilterWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

ProteomicsFilteringDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(proteomicsFilter1 = NULL,
                   proteomicsFilter2 = NULL
                   ),
    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            message(sprintf("initializing demo"))
            private$proteomicsFilter1 <- ProteomicsFilteringWidget$new(id="d3.foo")
            private$proteomicsFilter2 <- ProteomicsFilteringWidget$new(id="d3.bar")
            },

        #------------------------------------------------------------
        ui = function(){


           fluidPage(
                  tabsetPanel(
                     tabPanel("Filtering", private$proteomicsFilter1$ui()),
                     tabPanel("Correlations", private$proteomicsFilter2$ui())
                     ) # tabsetPanel
              ) # fluidPage
            },

        #------------------------------------------------------------
        server = function(input, output, session){
            message(sprintf("entering ProteomicsFilterWidgetDemo::server"))
            private$proteomicsFilter1$server(input, output, session)
            message(sprintf("leaving ProteomicsFilterWidgetDemo::server"))
            private$proteomicsFilter2$server(input, output, session)
            #message(sprintf("entering ProteomicsFilterWidgetDemo::server"))
            }
       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- ProteomicsFilteringDemoApp$new()


