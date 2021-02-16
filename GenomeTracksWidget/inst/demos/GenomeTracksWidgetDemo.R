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
   Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS=TRUE)
   install_github("paul-shannon/igvShiny", force=TRUE)
   install_github("PriceLab/BioShiny/igvWidget", force=TRUE)
   install_github("PriceLab/BioShiny/GenomeTracksWidget", force=TRUE)

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
filename <- "~/github/TrenaProjectErythropoiesis/inst/extdata/genomicRegions/tbl.3.0.250000.500000.RData"
#filename <- "../extdata/tbl-79.rows.RData"
#filename <- "../extdata/tbl-21.rows.RData"
#filename <- "../extdata/tbl-66.rows.RData"
tbl <- get(load(filename))

app <- GenomeTracksWidgetDemoApp$new(tbl)

if(grepl("hagfish", Sys.info()[["nodename"]]) & !interactive()){
   runApp(shinyApp(app$ui(), app$server), port=1118)
   } else {
   shinyApp(app$ui(), app$server)
   }



