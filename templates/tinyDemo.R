library(shiny)
library(R6)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; font-size: 20px; background-color:white"
textOutputStyle <- paste0("margin:10px; margin-left: 50px;",
		          " padding:5px; width: 200px; height: 60px; color:red; ",
		          "border: 1px solid black; font-size: 20px;")
DemoApp = R6Class("DemoApp",

    #--------------------------------------------------------------------------------
    private = list(latestText=NULL
                   ),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            print(noquote(sprintf("initializing demo")))
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              wellPanel(style="width: 1000px;",
                actionButton("randomTextButton", label= "Generate random text",
                             style=buttonStyle)),
                div(style=textOutputStyle, textOutput("textDisplay"))

            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            print(noquote(sprintf("entering server")))

            observeEvent(input$randomTextButton, ignoreInit=TRUE, {
              randomText <- paste(sample(c(LETTERS, letters), 10, replace=TRUE), collapse="")
              private$latestText <- randomText;  # though not currently reused
	      output$textDisplay <- renderText(randomText);
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

   deployApp(account="paulshannon",
              appName="tinyDemo",
              appTitle="tiny Demo",
              appFiles=c("tinyDemo.R"),
              appPrimaryDoc="tinyDemo.R"
              )

} # deploy
#------------------------------------------------------------------------------------------------------------------------
app.obj <- DemoApp$new()
if(grepl("hagfish", Sys.info()[["nodename"]])) { # & !interactive()){
   runApp(shinyApp(app.obj$ui, app.obj$server), port=6867)
   } else {
   shinyApp(app.obj$ui, app.obj$server)
   }
