library(R6)
library(shiny)
library(HeatmapWidget)
library(shinyWidgets)
options(warn=2)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

HeatmapDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(heatmap = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
           message(sprintf("initializing demo"))
           rowGroups <- rep("other", nrow(mtcars))
           rowGroups[grep("^M", rownames(mtcars))] <- "M"
           rowGroups[grep("^F", rownames(mtcars))] <- "F"
           rowGroups[grep("^D", rownames(mtcars))] <- "D"
           tbl.rowGroups <- data.frame(LETTER=rowGroups)
           colGroups <- nchar(colnames(mtcars))
           tbl.colGroups <- data.frame(nchar=colGroups)
           rowGroupColors <- list(LETTER="Set1")
           colGroupColors <- list(nchar="Dark2")
           private$heatmap = HeatmapWidget$new(id="box1",
                                               title="mtcars",
                                               mtx=as.matrix(mtcars),
                                               rowTitle="model",
                                               columnTitle="attributes",
                                               rowGroups=tbl.rowGroups,
                                               colGroups=tbl.colGroups,
                                               rowGroupColors=rowGroupColors,
                                               colGroupColors=colGroupColors,
                                               width=1000,
                                               height=800)
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
                private$heatmap$ui(),
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

           message(sprintf("entering heatmapWidgetDemo::server"))
           private$heatmap$server(input, output, session)
           } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
deploy <- function()
{
   repos <- options("repos")[[1]]
   stopifnot(sort(names(repos)) == c("BioCann", "BioCsoft", "CRAN"))
   stopifnot(repos$BioCann=="https://bioconductor.org/packages/3.12/data/annotation")
   stopifnot(repos$BioCsoft=="https://bioconductor.org/packages/3.12/bioc")
   stopifnot(repos$CRAN=="https://cran.microsoft.com")

   require(devtools)
   install_github("ropensci/iheatmapr", force=TRUE)
   install_github("PriceLab/BioShiny/HeatmapWidget", force=TRUE)

   require(rsconnect)

   deployApp(account="hoodlab",
             appName="HeatmapWidgetDemo1",
             appTitle="Heatmap shiny widget, built upon iheatmap",
             appFiles=c("HeatmapWidgetDemo1.R"),
             appPrimaryDoc="HeatmapWidgetDemo1.R",
             forceUpdate=TRUE
             )


} # deploy
#--------------------------------------------------------------------------------
app <-  HeatmapDemoApp$new()
if(grepl("hagfish", Sys.info()[["nodename"]]) & !interactive()){
   runApp(shinyApp(app$ui(), app$server), port=1114)
   } else {
   shinyApp(app$ui(), app$server)
   }
