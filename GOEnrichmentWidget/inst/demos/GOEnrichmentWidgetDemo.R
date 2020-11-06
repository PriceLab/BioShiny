library(R6)
library(shiny)
library(GOEnrichmentWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

GOEnrichmentWidgetDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(goWidget = NULL,
                   genes = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(geneSymbols){
            printf("initializing demo")
            private$genes <- geneSymbols
            private$goWidget = GOEnrichmentWidget$new(id="goWidget.1", title="GO Enrichment",
                                                      geneSymbols=geneSymbols)
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
               wellPanel(
                  actionButton("subsetGenesButton", "Subset")
                   ),
              private$goWidget$ui()
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            printf("entering GOEnricmentWidgetDemo::server")
            private$goWidget$server(input, output, session)

            observeEvent(input$subsetGenesButton, ignoreInit=TRUE, {
              gene.subset <- sample(private$genes, size=5)
              private$goWidget$setGenes(gene.subset)
              private$goWidget$run(gene.subset)

              })
            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
goi <- c("MIP", "AFP", "CAMK2N2", "CALR3", "TEX19", "CLCA2", "LINC01602", "CYP4F11",
         "MIR5581", "ENTD1", "LUM", "DRD2", "ZNF560", "MMP12", "TMEM26", "AKAP4")
app <- GOEnrichmentWidgetDemoApp$new(geneSymbols=goi)
x <- shinyApp(app$ui, app$server)
runApp(x, port=1112)

