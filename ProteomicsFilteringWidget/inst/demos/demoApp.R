#library(R6)
#library(shiny)
library(ProteomicsFilterWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

ProteomicsFilteringDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(proteomicsFilter1 = NULL,
                   proteomicsFilter2 = NULL,
                   randomTextButton = NULL,
                   getTextButton = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            message(sprintf("initializing demo"))
            private$proteomicsFilter1 = ProteomicsFilteringWidget$new(id="box1", title="random mixed text")
            private$proteomicsFilter2 = ProteomicsFilteringWidget$new(id="box2", title="lower case", boxWidth=400, boxHeight=50,
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
                      tags$li('ProteomicsFilterWidget$new(id="box1", title="random mixed text")'),
                      tags$li('ProteomicsFilterWidget$new(id="box2", title="lower case", boxWidth=400, boxHeight=50,
                              fontSize=30, backgroundColor="lightgray")'))),

              wellPanel(style="width: 1000px; height: 100px;",
                actionButton("randomTextButton", label= "Generate random Text",
                             style="margin-top: 10px; margin-left: 40px;"),
                actionButton("getTextButton", label= "Get Text (to stdout)",
                             style="margin-top: 10px; margin-left: 40px;")),
                private$proteomicsFilter1$ui(),
                private$proteomicsFilter2$ui()
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            message(sprintf("entering ProteomicsFilterWidgetDemo::server"))
            private$proteomicsFilter1$server(input, output, session)
            private$proteomicsFilter2$server(input, output, session)

            observeEvent(input$randomTextButton, ignoreInit=TRUE, {
              randomText <- paste(sample(c(LETTERS, letters), 10, replace=TRUE), collapse="")
              private$proteomicsFilter1$setText(randomText)
              private$proteomicsFilter2$setText(tolower(randomText))
              private$proteomicsFilter1$getText() == randomText
              private$proteomicsFilter2$getText() == tolower(randomText)
              })

            observeEvent(input$getTextButton, ignoreInit=TRUE, {
              print(private$proteomicsFilter1$getText())
              })

            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- ProteomicsFilteringDemoApp$new()

if(grepl("hagfish", Sys.info()[["nodename"]]) & interactive()){
   printf("--- on hagfish")
   runApp(shinyApp(app$ui(), app$server), port=1112)
   } else {
   shinyApp(app$ui(), app$server)
   }

