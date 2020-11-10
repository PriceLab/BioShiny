library(R6)
library(shiny)
library(HeatmapWidget)
library(shinyWidgets)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

HeatmapDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
     private = list(heatmapWidget = NULL,
                    mtx=NULL,
                    k.row.clusters=NULL,
                    k.column.clusters=NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            mtx.file.1 <- "genesByScoredCandidateTFs.mtx.16x113.RData"
            mtx.file.2 <- "genesByScoredCandidateTFs.mtx.22x340.RData"
            mtx <- get(load(system.file(package="HeatmapWidget", "extdata", mtx.file.2)))
            private$mtx <- mtx
            private$k.row.clusters <- 3
            private$k.column.clusters <- 3
            private$heatmapWidget = HeatmapWidget$new(id="box1",
                                                      title="Coordinate Regulation",
                                                      mtx,
                                                      rowClusters=private$k.row.clusters,
                                                      colClusters=private$k.column.clusters,
                                                      rowTitle="Gene",
                                                      columnTitle="TF",
                                                      width="100%",
                                                      height=700)
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              titlePanel("Coordinate regulation of early hematopoiesis genes"),
              fluidRow(column(width=3, pickerInput(inputId = "rowClusterPicker",
                                                   label = "K Row Clusters",
                                                   choices = as.character(2:10),
                                                   selected = as.character(private$k.row.clusters),
                                                   options = list(`actions-box`=TRUE, size=5,`selected-text-format`="count > 3"),
                                                   multiple = FALSE
                                                   )),
                       column(width=3, pickerInput(inputId = "columnClusterPicker",
                                                   label = "K Column Clusters",
                                                   choices = as.character(2:10),
                                                   selected = as.character(private$k.column.clusters),
                                                   options = list(`actions-box`=TRUE, size=5,`selected-text-format`="count > 3"),
                                                   multiple = FALSE
                                                   ))),

                   #radioButtons("matrixTransformChooser",
                   #             "Matrix transform",
                   #             choices=c("none", "asinh", "log10"),
                   #             selected="none",
                   #             inline = TRUE)
                   #), # wellPanel
                private$heatmapWidget$ui(),
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            printf("entering heatmapWidgetDemo::server")
            private$heatmapWidget$server(input, output, session)

            observeEvent(input$heatmap_click, ignoreInit=TRUE, {
               printf("=== demo2 sees heatmap click")
               x <- input$heatmap_click
               print(x)
               })
            observeEvent(input$heatmap_brush, ignoreInit=TRUE, {
                printf("=== demo2 sees heatmap brush")
                x <- input$heatmap_brush
                print(x)
                })

            observeEvent(input$matrixTransformChooser, ignoreInit=TRUE, {
               newTransform <- input$matrixTransformChooser
               printf("new transform: %s", newTransform)
               mtx.transformed <- switch(newTransform,
                                         "asinh" = asinh(private$mtx),
                                         "none"  = private$mtx,
                                         "log10" = log10(private$mtx + 0.0001))
               private$heatmapWidget$setHeatmap(mtx.transformed, )
               })

            observeEvent(input$rowClusterPicker, ignoreInit=TRUE, {
               private$k.row.clusters <- input$rowClusterPicker
               private$heatmapWidget$setHeatmap(private$mtx,
                                                rowClusters=private$k.row.clusters,
                                                colClusters=private$k.column.clusters)
               })

            observeEvent(input$columnClusterPicker, ignoreInit=TRUE, {
               private$k.column.clusters <- input$columClusterPicker
               private$heatmapWidget$setHeatmap(private$mtx,
                                                rowClusters=private$k.row.clusters,
                                                colClusters=private$k.column.clusters)
               })


            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- HeatmapDemoApp$new()
x <- shinyApp(app$ui, app$server)
runApp(x, port=1112)

