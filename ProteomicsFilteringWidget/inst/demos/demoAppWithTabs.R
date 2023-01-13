#library(R6)
#library(shiny)
library(ProteomicsFilterWidget)
#f <- system.file(package="ProteomicsFilterWidget", "extdata", "tbl.all-11492x14.RData")
f <- system.file(package="ProteomicsFilterWidget", "extdata", "tbl.all-dia-rna.38662-14.RData")
tbl.all <- get(load(f))
#f <- system.file(package="ProteomicsFilterWidget", "extdata", "tbl.complexes.RData")
f <- file.path("~/github/BioShiny/ProteomicsFilteringWidget/inst/extdata", "tbl.complexes-12jan2023.RData")
tbl.complexes <- get(load(f))
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
            private$proteomicsFilter1 <- ProteomicsFilteringWidget$new(id="d3.foo", tbl.all, tbl.complexes)
            private$proteomicsFilter2 <- ProteomicsFilteringWidget$new(id="d3.bar", tbl.all, tbl.complexes)
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
# shinyApp(app$ui(), app$server)
if(grepl("hagfish", Sys.info()[["nodename"]])){ #  & interactive()){
   printf("--- on hagfish")
   runApp(shinyApp(app$ui(), app$server), port=1112)
   } else {
   shinyApp(app$ui(), app$server)
   }



