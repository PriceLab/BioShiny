library(R6)
library(shiny)
library(dataTableWidget)
library(msgBoxWidget)
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
            printf("initializing demo")
            private$msgBox =  msgBoxWidget$new(id="box1", title="table selection", boxWidth=600)
            private$tbl = get(load("../extdata/tableWithVoluminousCellText.RData"))
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

            printf("entering dataTableWidgetDemo::server")
            private$msgBox$server(input, output, session)
            private$dtw$server(input, output, session)

            observeEvent(input$rowWrappedStateButtons, ignoreInit=TRUE, {
               printf("button click")
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
app <- DataTableDemoApp$new()
#x <- shinyApp(app$ui, app$server)a
runApp(shinyApp(app$ui, app$server), port=1112)

