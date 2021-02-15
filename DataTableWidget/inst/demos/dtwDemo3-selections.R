library(R6)
library(shiny)
library(DataTableWidget)
library(MsgBoxWidget)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

DataTableDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(tbl = NULL,
                   msgBox.1 = NULL,
                   msgBox.2 = NULL,
                   msgBox.3 = NULL,
                   dtw1 = NULL,
                   dtw2 = NULL,
                   dtw3 = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            private$msgBox.1 =  msgBoxWidget$new(id="box1", title="selected cells", boxWidth=600)
            private$msgBox.2 =  msgBoxWidget$new(id="box2", title="selected columns", boxWidth=600)
            private$msgBox.3 =  msgBoxWidget$new(id="box3", title="selected rows", boxWidth=600)
            private$tbl = mtcars
            private$dtw1 = dataTableWidget$new(id="dtw1", private$tbl, width="80%",
                                              height="250px", border="1px red solid; border-radius: 5px;",
                                              selectionMode="single",
                                              selectionTarget="cell",
                                              pageLength=12)
            private$dtw2 = dataTableWidget$new(id="dtw2", private$tbl, width="80%",
                                              height="250px", border="1px red solid; border-radius: 5px;",
                                              selectionMode="single",
                                              selectionTarget="column",
                                              pageLength=12)
            private$dtw3 = dataTableWidget$new(id="dtw3", private$tbl, width="80%",
                                              height="250px", border="1px red solid; border-radius: 5px;",
                                              selectionMode="single",
                                              selectionTarget="row",
                                              pageLength=12)
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
               wellPanel(style="width: 1000px;",
                   fluidRow(
                       column(2, radioButtons("wrapOrNoWrap", "Wrap text in rows", choices=c("yes", "no"), selected="no")),
                       column(2, selectInput("carSelector", "Select Car", c("", rownames(mtcars)))),
                       column(2, div(actionButton("selectionColumnsButton", "Select Columns")), style="margin-top:30px;"),
                       column(1, div(actionButton("clearSelectionButton", "Clear")), style="margin-top:30px;"),
                       column(2, div(actionButton("randomSubsetButton", "Random subset")), style="margin-top:30px;"),
                       column(2, div(actionButton("fullTableButton", "Full Table")), style="margin-top:30px;")
                       )
                    ),
                div(private$msgBox.1$ui(), style="margin:5px"),
                private$dtw1$ui(),
                div(private$msgBox.2$ui(), style="margin:5px"),
                private$dtw2$ui(),
                div(private$msgBox.3$ui(), style="margin:5px"),
                private$dtw3$ui()
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            private$msgBox.1$server(input, output, session)
            private$msgBox.2$server(input, output, session)
            private$msgBox.3$server(input, output, session)

            private$dtw1$server(input, output, session)
            private$dtw2$server(input, output, session)
            private$dtw3$server(input, output, session)

            observeEvent(input$selectionColumnsButton, ignoreInit=TRUE, {
                printf("select columns")
                private$dtw1$selectColumns(5:6)
                })

            observeEvent(input$clearSelectionButton, ignoreInit=TRUE, {
                private$dtw1$selectRows(NULL)
                private$msgBox.1$setText("")
                private$msgBox.2$setText("")
                private$msgBox.3$setText("")
                })

            observeEvent(input$randomSubsetButton, ignoreInit=TRUE, {
                rows <- sort(sample(seq_len(nrow(private$tbl)), 10))
                tbl.sub <- private$tbl[rows,]
                private$dtw1$setTable(tbl.sub)
                })

            observeEvent(input$fullTableButton, ignoreInit=TRUE, {
                private$dtw1$setTable(private$tbl)
                })

            observeEvent(input$carSelector, ignoreInit=TRUE, {
                car <- input$carSelector
                req(car)
                printf("car: %s", car)
                private$dtw1$selectRows(grep(car, rownames(private$tbl)))
                })

            observe({
               selection <- private$dtw1$tableSelection()
               req(selection)
               row.col <- as.integer(selection)
               row.name <- rownames(private$tbl)[row.col[1]]
               col.name <- colnames(private$tbl)[row.col[2]]
               private$msgBox.1$setText(sprintf("%s  %s   %s", row.name, col.name,
                                                paste(selection, collapse=", ")))
               })

            observe({
               selection <- private$dtw2$tableSelection()
               print(selection)
               private$msgBox.2$setText(paste(selection, collapse=", "))
               })

            observe({
               selection <- private$dtw3$tableSelection()
               print(selection)
               private$msgBox.3$setText(paste(selection, collapse=", "))
               })

            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- DataTableDemoApp$new()
runApp(shinyApp(app$ui, app$server), port=1112)

