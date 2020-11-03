library(R6)
library(shiny)
library(HeatmapWidget)
options(warn=2)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

HeatmapDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(heatmap = NULL,
                   input = NULL,
                   output = NULL,
                   session = NULL,
                   mtx = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              titlePanel("Load heatmaps dynamically"),
              sidebarLayout(
                 sidebarPanel(
                    actionButton("mtcarsButton", "mtcars"),
                    actionButton("tfGenesButton", "TFs and Genes"),
                    wellPanel(
                        htmlOutput("heatmapClickReadout"),
                        style="background-color: white; margin-top: 10px; font-size: 18px;"),
                    width=2),
                 mainPanel(
                    div(id="heatmapContainer", div(id="heatmapWrapper")),
                    width=10
                   )
                ) # sidebarLayout
              ) # fluidPage
           },

        #------------------------------------------------------------
        server = function(input, output, session){

           private$input <- input
           private$output <- output
           private$session <- session

             #-------------------------------
             #  mtcars button click handler
             #-------------------------------
           observeEvent(input$mtcarsButton, ignoreInit=TRUE, {
              private$mtx <- mtcars
              private$heatmap = HeatmapWidget$new(id="box1",
                                                  title="mtcars",
                                                  mtcars,
                                                  width=500,
                                                  height=500)
              removeUI(selector="#heatmapWrapper", immediate=TRUE)
              insertUI(selector="#heatmapContainer", where="beforeEnd",
                       div(id="heatmapWrapper"), immediate=TRUE)
              insertUI(selector="#heatmapWrapper", where="beforeEnd", private$heatmap$ui(), immediate=TRUE)
              private$heatmap$server(input, output, session)
              })

             #-------------------------------------------
             #  TFs & targetGenes  button click handler
             #-------------------------------------------
           observeEvent(input$tfGenesButton, ignoreInit=TRUE, {
              mtx.file.2 <- "genesByScoredCandidateTFs.mtx.22x340.RData"
              mtx <- asinh(get(load(system.file(package="HeatmapWidget", "extdata", mtx.file.2))))
              private$mtx <- mtx
              private$heatmap = HeatmapWidget$new(id="box2",
                                                  title="TFs and Targets",
                                                  mtx,
                                                  width=700,
                                                  height=700)
              removeUI(selector="#heatmapWrapper", immediate=TRUE)
              insertUI(selector="#heatmapContainer", where="beforeEnd",
                       div(id="heatmapWrapper"), immediate=TRUE)
              insertUI(selector="#heatmapWrapper", where="beforeEnd", private$heatmap$ui(), immediate=TRUE)
              private$heatmap$server(input, output, session)
              })

            eventMessage <- reactiveVal()
            output$heatmapClickReadout <- renderUI(HTML(eventMessage()))


            observeEvent(input$heatmap_click, ignoreInit=TRUE, {
               x <- input$heatmap_click
               click.info <- private$heatmap$clickEventAsPosition(x)
               if(length(click.info) == 0) return()
               tbl.clickInfo <- as.data.frame(click.info)
               row <- tbl.clickInfo$row_index[1]
               col <- tbl.clickInfo$column_index[1]
               eventMessage(sprintf("%s, %s: %5.2f",
                                    rownames(private$mtx)[row],
                                    colnames(private$mtx)[col],
                                    private$mtx[row, col]))
               })
            #observeEvent(input$heatmap_brush, ignoreInit=TRUE, {
            #    printf("=== demo3 sees heatmap brush")
            #    x <- input$heatmap_brush
            #    print(x)
            #    })

            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- HeatmapDemoApp$new()
x <- shinyApp(app$ui, app$server)
runApp(x, port=1112)

