library(R6)
library(shiny)
library(DataTableWidget)
library(MsgBoxWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

DataTableDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(dtWidget.1 = NULL,
                   randomTextButton = NULL,
                   tbl = NULL,
                   msgBox = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            private$msgBox =  msgBoxWidget$new(id="box1", title="table selection", boxWidth=600)
            private$tbl = mtcars
            private$dtWidget.1 = dataTableWidget$new(id="tbl.1", private$tbl, width="80%",
                                                     height="300px", border="1px red solid; border-radius: 5px;",
                                                     pageLength=12)
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
               wellPanel(style="width: 1000px;",
                         private$msgBox$ui(),
                         fluidRow(
                             column(2, radioButtons("wrapOrNoWrap", "Wrap text in rows", choices=c("yes", "no"), selected="no")),
                             column(2, selectInput("carSelector", "Select Car", c("", rownames(mtcars)))),
                             column(1, div(actionButton("clearSelectionButton", "Clear")), style="margin-top:30px;"),
                             column(2, div(actionButton("randomSubsetButton", "Random subset")), style="margin-top:30px;"),
                             column(2, div(actionButton("fullTableButton", "Full Table")), style="margin-top:30px;")
                             )
                        ),
                private$dtWidget.1$ui()
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            private$msgBox$server(input, output, session)
            private$dtWidget.1$server(input, output, session)

            observeEvent(input$clearSelectionButton, ignoreInit=TRUE, {
                private$dtWidget.1$selectRows(NULL)
                private$msgBox$setText("")
                })

            observeEvent(input$randomSubsetButton, ignoreInit=TRUE, {
                rows <- sort(sample(seq_len(nrow(private$tbl)), 10))
                tbl.sub <- private$tbl[rows,]
                private$dtWidget.1$setTable(tbl.sub)
                })

            observeEvent(input$fullTableButton, ignoreInit=TRUE, {
                private$dtWidget.1$setTable(private$tbl)
                })

            observeEvent(input$carSelector, ignoreInit=TRUE, {
                car <- input$carSelector
                req(car)
                printf("car: %s", car)
                private$dtWidget.1$selectRows(grep(car, rownames(private$tbl)))
                })

            observe({
               row.names <- private$dtWidget.1$tableSelection()
               print(row.names)
               private$msgBox$setText(paste(row.names, collapse=", "))
               })
            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- DataTableDemoApp$new()
#x <- shinyApp(app$ui, app$server)a
runApp(shinyApp(app$ui, app$server), port=1112)

