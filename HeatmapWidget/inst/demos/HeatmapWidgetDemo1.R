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
            private$heatmap = HeatmapWidget$new(id="box1",
                                                title="mtcars",
                                                mtx=as.matrix(mtcars),
                                                rowTitle="model",
                                                columnTitle="attributes",
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
# need this first:
#    install_github("ropensci/iheatmapr")
#    install_github("PriceLab/BioShiny/HeatmapWidget")
deploy <- function()
{
   require(rsconnect)
   #rsconnect::setAccountInfo(name='hoodlab',
   #                          token='41E779ABC50F6A98036C95AEEA1A92F7',
   #                          secret='PDVweDhzJa8ST3fu5zihMEKVcPH0cssByz7Q6rsL')
  # setRepositories(addURLs=c(BioCsoft="https://bioconductor.org/packages/3.12/bioc",
  #                           BioCann="https://bioconductor.org/packages/3.12/data/annotation",
  #                           BioCexp="https://bioconductor.org/packages/3.12/data/experiment",
  #                           BioC="https://bioconductor.org/packages/3.12/bioc",
  #                           CRAN="https://cran.microsoft.com"),
  #-                 graphics=FALSE)

   deployApp(account="hoodlab",
             appName="HeatmapWidgetDemo1",
             appTitle="Heatmap shiny widget, built upon iheatmap",
             appFiles=c("HeatmapWidgetDemo1.R"),
             appPrimaryDoc="HeatmapWidgetDemo1.R"
             )


} # deploy
#--------------------------------------------------------------------------------
app <-  HeatmapDemoApp$new()
if(grepl("hagfish", Sys.info()[["nodename"]]) & !interactive()){
   runApp(shinyApp(app$ui(), app$server), port=1114)
   } else {
   shinyApp(app$ui(), app$server)
   }
