#' R6 Class representing a simple text display widget
#'
#' @import R6
#' @import iheatmapr
#'
#' @title HeatmapWidget
#' @description an R6 class providing clean access to iheatmapr
#' @name HeatmapWidget
#'
printf <- function(...) print(noquote(sprintf(...)))

#' @export
HeatmapWidget = R6Class("HeatmapWidget",

    private = list(
        id = NULL,
        title = NULL,
        mtx = NULL,
        rowTitle = NULL,
        columnTitle = NULL,
        clusteringMethod = NULL,
        rowClusters = NULL,
        colClusters = NULL,
        width = NULL,
        height = NULL,
        quiet = NULL,
        input = NULL,
        output = NULL,
        session = NULL,
        heatmap = NULL,
        reactive.heatmap = NULL
        ),

        #' @description
        #' Create a new HeatmapWidget
        #' @param id the html document div id
        #' @param title character
        #' @param mtx a numeric matrix
        #' @param rowTitle character, displayed on far left of the plot
        #' @param colTitle character, displayed at the bottom
        #' @param rowClusters integer, the number of k-means row clusters desired, default 5
        #' @param colClusters integer, the number of k-means column clusters desired, default 5
        #' @param width numeric, pixels, default 600
        #' @param height numeric, pixels, default 600
        #' @param quiet logical silent or verbose?  default TRUE
        #'
        #' @return A new `HeatmapWidget` object.

   public = list(
       initialize = function(id, title, mtx, rowTitle, columnTitle,
                             clusteringMethod="hclust",
                             rowClusters=5, colClusters=5,
                             width=600, height=600, quiet=TRUE){

          if(!quiet) message("entering HeatmapWidget::initialize")
          private$id = id;
          private$title = title;
          private$mtx = mtx;
          private$rowTitle = rowTitle
          private$clusteringMethod = clusteringMethod
          private$columnTitle = columnTitle
          private$rowClusters = rowClusters
          private$colClusters = colClusters
          private$width = width;
          private$height = height;
          private$quiet = quiet
          #public$setHeatmap(mtx)
          printf("--- HeatmapWidget ctor, checking rows")
          if(nrow(mtx) == 1)   # hclust needs at least two rows.
              mtx <- rbind(mtx, mtx)
          if(ncol(mtx) == 1)   # hclust needs at least two rows.
              mtx <- cbind(mtx, mtx)
          printf("ctor calls main_heatmap, mtx rows: %d", nrow(mtx))
          hm <- main_heatmap(mtx)
          hm <- add_col_clustering(hm, method="hclust", show_colorbar=FALSE)
          hm <- add_row_clustering(hm, method="hclust")
          hm <- add_col_labels(hm)
          hm <- add_row_labels(hm)
          hm <- add_row_title(hm, rowTitle)
          hm <- add_col_title(hm, columnTitle)
          private$heatmap <- hm
          },

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
          fluidRow(
             iheatmaprOutput(private$id, width=private$width, height=private$height)
             ) # fluidPage
          }, # ui

        #' @description
        #' defines the event handlers used with the ui
        #' @param input list, managed by Shiny
        #' @param output list, managed by Shiny
        #' @param session list (environment?) managed by Shiny
        #' @returns nothing
      server = function(input, output, session){

         private$session = session;
         private$input = input
         private$output = output

         private$reactive.heatmap <- reactiveVal(private$heatmap)

         output[[private$id]] = renderIheatmap({
            printf("--- renderIheatmap")
            private$reactive.heatmap()
            })

         observe({
            #s <- iheatmapr_event(private$heatmap, event="click") #,"hover","relayout"))
            printf("--- HeatmapWidget click event")
            })

         observe({
             #s <- iheatmapr_event(private$heatmap, event="relayout")
             printf("--- HeatmapWidget relayout event")
             })
         }, # server

        #' @description
        #' new heatmap displayed
        #' @param input list, managed by Shiny
        #' @export
      setHeatmap = function(mtx, clusteringMethod="hclust", rowClusters=5, colClusters=5){
         print(-2)
         private$mtx <- mtx
         print(-1)
         if(nrow(private$mtx) == 1)   # hclust needs at least two rows.
             private$mtx <- rbind(private$mtx, private$mtx)
         if(ncol(private$mtx) == 1)   # hclust needs at least two cols
             private$mtx <- cbind(private$mtx, private$mtx)
         print(1)
         printf("incoming row: %d, adjusted rows: %d", nrow(mtx), nrow(private$mtx))
         private$clusteringMethod <- clusteringMethod
         print(2)
         private$rowClusters <- rowClusters
         print(3)
         if(private$rowClusters > nrow(private$mtx))
             private$rowClusters <- 2
         print(4)
         private$colClusters <- colClusters
         hm <- main_heatmap(private$mtx)
         hm <- add_col_clustering(hm, method="hclust", show_colorbar=FALSE)
         hm <- add_row_clustering(hm, method="hclust", show_colorbar=FALSE)
         #hm <- add_col_clustering(hm, method=private$clusteringMethod, k=private$colClusters, show_colorbar=FALSE)
         #hm <- add_row_clustering(hm, method=private$clusteringMethod, k=private$rowClusters, show_colorbar=FALSE)

         hm <- add_col_labels(hm)
         hm <- add_row_labels(hm)
         hm <- add_row_title(hm, private$rowTitle)
         hm <- add_col_title(hm, private$columnTitle)
         private$heatmap <- hm
         private$reactive.heatmap(hm)
         } # setHeatmap

     ) # public

  ) # class

