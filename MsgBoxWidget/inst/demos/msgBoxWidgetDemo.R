library(R6)
library(shiny)
library(MsgBoxWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

MsgBoxDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(msgBox1 = NULL,
                   msgBox2 = NULL,
                   randomTextButton = NULL,
                   getTextButton = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            message(sprintf("initializing demo"))
            private$msgBox1 = msgBoxWidget$new(id="box1", title="random mixed text")
            private$msgBox2 = msgBoxWidget$new(id="box2", title="lower case", boxWidth=400, boxHeight=50,
                 fontSize=30, backgroundColor="lightgray")
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
               div(id="foo",
                   style="margin: 20px; padding: 10px;
                          border: 1px solid grey;
                          border-radius: 3px;
                          height: 120px; width: 1000px;",
                   h5("A simple R6 class for read-only text message display, constructor calls:"),
                   tags$ul(
                      tags$li('msgBoxWidget$new(id="box1", title="random mixed text")'),
                      tags$li('msgBoxWidget$new(id="box2", title="lower case", boxWidth=400, boxHeight=50,
                              fontSize=30, backgroundColor="lightgray")'))),

              wellPanel(style="width: 1000px; height: 100px;",
                actionButton("randomTextButton", label= "Generate random Text",
                             style="margin-top: 10px; margin-left: 40px;"),
                actionButton("getTextButton", label= "Get Text (to stdout)",
                             style="margin-top: 10px; margin-left: 40px;")),
                private$msgBox1$ui(),
                private$msgBox2$ui()
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            message(sprintf("entering msgBoxWidgetDemo::server"))
            private$msgBox1$server(input, output, session)
            private$msgBox2$server(input, output, session)

            observeEvent(input$randomTextButton, ignoreInit=TRUE, {
              randomText <- paste(sample(c(LETTERS, letters), 10, replace=TRUE), collapse="")
              private$msgBox1$setText(randomText)
              private$msgBox2$setText(tolower(randomText))
              private$msgBox1$getText() == randomText
              private$msgBox2$getText() == tolower(randomText)
              })

            observeEvent(input$getTextButton, ignoreInit=TRUE, {
              print(private$msgBox1$getText())
              })

            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
deploy <-function()
{
   repos <- options("repos")[[1]]
   stopifnot(sort(names(repos)) == c("BioCann", "BioCsoft", "CRAN"))
   stopifnot(repos$BioCann=="https://bioconductor.org/packages/3.12/data/annotation")
   stopifnot(repos$BioCsoft=="https://bioconductor.org/packages/3.12/bioc")
   stopifnot(repos$CRAN=="https://cran.microsoft.com")

   require(devtools)
   install_github("PriceLab/BioShiny/MsgBoxWidget", force=TRUE)

   require(rsconnect)

   deployApp(account="hoodlab",
              appName="msgBoxDemo",
              appTitle="msgBox Demo",
              appFiles=c("msgBoxWidgetDemo.R"),
              appPrimaryDoc="msgBoxWidgetDemo.R",
              forceUpdate=TRUE
              )

} # deploy
#------------------------------------------------------------------------------------------------------------------------
app <- MsgBoxDemoApp$new()

if(grepl("hagfish", Sys.info()[["nodename"]]) & !interactive()){
   runApp(shinyApp(app$ui(), app$server), port=1112)
   } else {
   shinyApp(app$ui(), app$server)
   }

