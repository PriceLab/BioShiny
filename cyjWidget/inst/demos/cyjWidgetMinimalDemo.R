library(shiny)
library(R6)
library(cyjWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; font-size: 20px; background-color:white"
textOutputStyle <- paste0("margin:20px; margin-left: 50px;",
		          " padding:5px; width: 200px; height: 60px; color:red; ",
		          "border: 1px solid black; font-size: 20px;")
tbl.nodes <- data.frame(id=c("A", "B", "C"),
                        type=c("kinase", "TF", "glycoprotein"),
                        lfc=c(-3, 1, 1),
                        count=c(0, 0, 0),
                        stringsAsFactors=FALSE)

tbl.edges <- data.frame(source=c("A", "B", "C"),
                        target=c("B", "C", "A"),
                        interaction=c("phosphorylates", "synthetic lethal", "unknown"),
                        stringsAsFactors=FALSE)

graph.json <- toJSON(dataFramesToJSON(tbl.edges, tbl.nodes), auto_unbox=TRUE)
#----------------------------------------------------------------------------------------------------
DemoApp = R6Class("DemoApp",

    #--------------------------------------------------------------------------------
    private = list(latestText=NULL,
                   cyj.01=NULL
                   ),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            private$cyj.01 <- cyjWidget$new("cyj.01", graph.json, width="100%", height="1000px", border="")
            print(noquote(sprintf("after cyjWidget ctor")))
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              wellPanel(style="width: 100%;",
                 actionButton("fitButton", label= "Fit", style=buttonStyle)
                 ),
              div(style=textOutputStyle, textOutput("textDisplay")),
              div(private$cyj.01$ui(), style="scroll: auto; margin-top: 10px; border: 1px solid darkblue")
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            print(noquote(sprintf("entering server")))
            private$cyj.01$server(input, output, session)

            observeEvent(input$fitButton, ignoreInit=TRUE, {
               private$cyj.01$fitGraph()
               })
            } # server
       ) # public
    ) # class
#--------------------------------------------------------------------------------
deploy <-function()
{
   repos <- options("repos")[[1]]
   stopifnot(sort(names(repos)) == c("BioCann", "BioCsoft", "CRAN"))
   stopifnot(repos$BioCann=="https://bioconductor.org/packages/3.13/data/annotation")
   stopifnot(repos$BioCsoft=="https://bioconductor.org/packages/3.13/bioc")
   stopifnot(repos$CRAN=="https://cran.microsoft.com")
   require(devtools)

      # jim hester suggests, with reference
      # Setting R_REMOTES_NO_ERRORS_FROM_WARNINGS="false" will cause warning
      # messages during calls to install.packages() to become errors. Often warning
      # messages are caused by dependencies failing to install.
   Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")

   require(rsconnect)
   #rsconnect::showLogs(appName="tinyDemo", streaming=TRUE)

   deployApp(account="paulshannon",
              appName="tinyDemo",
              appTitle="tiny Demo",
              appFiles=c("tinyDemo.R"),
              appPrimaryDoc="tinyDemo.R"
              )

} # deploy
#------------------------------------------------------------------------------------------------------------------------
app.obj <- DemoApp$new()
if(grepl("hagfish", Sys.info()[["nodename"]]) & !interactive()){
   runApp(shinyApp(app.obj$ui, app.obj$server), port=6860)
   } else {
   shinyApp(app.obj$ui, app.obj$server)
   }

