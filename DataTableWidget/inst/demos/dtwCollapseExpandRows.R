library(R6)
library(shiny)
library(DataTableWidget)
library(MsgBoxWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

DataTableDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(dtw = NULL,
                   randomTextButton = NULL,
                   tbl = NULL,
                   msgBox = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
           message(sprintf("initializing demo"))
            private$msgBox =  msgBoxWidget$new(id="box1", title="table selection", boxWidth=600)
            private$tbl = get(load(system.file(package="dataTableWidget", "extdata",
                                               "tableWithVoluminousCellText.RData")))
            private$dtw = dataTableWidget$new(id="tbl.1",
                                              private$tbl,
                                              width="98%", height="500px",
                                              border="1px blue solid; border-radius: 10px;",
                                              columnWidths=c(50,60,70,80,90,100,110,120))
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              div(style="margin: 100px;",
                  wellPanel(style="width: 1000px;",
                            radioButtons(inputId="rowWrappedStateButtons",
                                         label="Wrap text within rows?",
                                         choices = c("Yes", "No"),
                                         selected = "No",
                                         inline = TRUE
                                         )),
                  private$dtw$ui()
                  ) # div
              )},

        #------------------------------------------------------------
        server = function(input, output, session){

           message(sprintf("entering dataTableWidgetDemo::server"))
            private$msgBox$server(input, output, session)
            private$dtw$server(input, output, session)

            observeEvent(input$rowWrappedStateButtons, ignoreInit=TRUE, {
              message(sprintf("button click"))
               newValue <- input$rowWrappedStateButtons
               private$dtw$setWrapLinesOption(newValue == "Yes")
               })

            observe({
               #row.names <- private$dtw$tableSelection()
               #row.names <- rownames(private$tbl)[row.numbers]
               #print(row.names)
               })
            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
# needs this first
deploy <- function()
{
   require(rsconnect)
   #rsconnect::setAccountInfo(name='hoodlab',
   #                          token='41E779ABC50F6A98036C95AEEA1A92F7',
   #                          secret='PDVweDhzJa8ST3fu5zihMEKVcPH0cssByz7Q6rsL')
   setRepositories(addURLs=c(BioCsoft="https://bioconductor.org/packages/3.12/bioc",
                             BioCann="https://bioconductor.org/packages/3.12/data/annotation",
                             BioCexp="https://bioconductor.org/packages/3.12/data/experiment",
                             BioC="https://bioconductor.org/packages/3.12/bioc",
                             CRAN="https://cran.microsoft.com"),
                   graphics=FALSE)

   deployApp(account="hoodlab",
              appName="dtwCollapseExpendRows",
              appTitle="dataTableWidget collapse/exapnd rows",
              appFiles=c("dtwCollapseExpandRows.R"),
              appPrimaryDoc="dtwCollapseExpandRows.R"
              )


} # deploy
#--------------------------------------------------------------------------------
app <- DataTableDemoApp$new()
if(grepl("hagfish", Sys.info()[["nodename"]]) & !interactive()){
   runApp(shinyApp(app$ui(), app$server), port=1113)
   } else {
   shinyApp(app$ui(), app$server)
   }
