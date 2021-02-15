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
            message(sprintf("initializing demo"))
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

            message(sprintf("entering GOEnricmentWidgetDemo::server"))
            private$goWidget$server(input, output, session)

            observeEvent(input$subsetGenesButton, ignoreInit=TRUE, {
              gene.subset <- sample(private$genes, size=5)
              private$goWidget$setGenes(gene.subset)
              #private$goWidget$run(gene.subset)
              })
            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
#   BiocManager::valid()
#
deploy <- function()
{
   repos <- options("repos")[[1]]
   stopifnot(sort(names(repos)) == c("BioCann", "BioCsoft", "CRAN"))
   stopifnot(repos$BioCann=="https://bioconductor.org/packages/3.12/data/annotation")
   stopifnot(repos$BioCsoft=="https://bioconductor.org/packages/3.12/bioc")
   stopifnot(repos$CRAN=="https://cran.microsoft.com")

   require(devtools)
   install_github("PriceLab/BioShiny/DataTableWidget", force=TRUE)
   install_github("PriceLab/BioShiny/GOEnrichmentWidget", force=TRUE)

   require(rsconnect)

   deployApp(account="hoodlab",
             appName="GOEnrichmentWidgetDemo",
             appTitle="GO Enrichment shiny Widget",
             appFiles=c("GOEnrichmentWidgetDemo.R"),
             appPrimaryDoc="GOEnrichmentWidgetDemo.R",
             forceUpdate=TRUE
             )


} # deploy
#--------------------------------------------------------------------------------
goi <- c("MIP", "AFP", "CAMK2N2", "CALR3", "TEX19", "CLCA2", "LINC01602", "CYP4F11",
         "MIR5581", "ENTD1", "LUM", "DRD2", "ZNF560", "MMP12", "TMEM26", "AKAP4")
app <- GOEnrichmentWidgetDemoApp$new(geneSymbols=goi)

if(grepl("hagfish", Sys.info()[["nodename"]]) & !interactive()){
   runApp(shinyApp(app$ui(), app$server), port=1114)
   } else {
   shinyApp(app$ui(), app$server)
   }
