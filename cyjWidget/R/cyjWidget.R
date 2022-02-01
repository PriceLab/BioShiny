#' R6 Class representing an igv.js genome browser
#'
#' @import R6
#' @import shiny
#' @import cyjShiny
#'
#' @title cyjsWidget
#' @description an R6 class providing clean access to the cyjShiny package
#' @name cyjWidget
#' @details
#' This class provides the cytoscape.js graph library in a classic object-oriented-programming R context
#'
#------------------------------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
#' @export
cyjWidget = R6Class("cyjWidget",

    private = list(
        id = NULL,
        graph.json = NULL,
        width = NULL,
        height = NULL,
        border = NULL,
        session = NULL
        ),

        #' @description
        #' Create a new cyjWidget
        #' @param id the html document div id
        #' @param graph.json a json graph
        #' @param width  integer, in width
        #' @param height integer, in pixels
        #' @param border character string, for instance "1px solid purple; border-radius: 5px;"
        #' @return A new `cyjWidget` object.

   public = list(

      trackClickResult = NULL,

      initialize = function(id, graph.json, width="95%", height="500px", border=""){
          private$id = id;
          private$graph.json = graph.json
          private$width = width;
          height.in.pixels <- as.integer(sub("px", "", height))
          height.in.pixels <- height.in.pixels + 50;
          private$height = height; # sprintf("%dpx", height.in.pixels)
          private$border = border;
          },

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
          tagList(
             div(id=sprintf("%s-container", private$id),
               cyjShinyOutput(private$id, private$width, private$height),
               style=sprintf("width: %s; height: %s; border: %s; overflow: auto; background-color: white;",
                             private$width, private$height, private$border))
             )
          },

        #' @description
        #' defines the event handlers used with the ui
        #' @param input list, managed by Shiny
        #' @param output  list, managed by Shiny
        #' @param session  list (environment?) managed by Shiny
        #' @returns nothing
      server = function(input, output, session){
         printf("--- starting cyjWidget$server")
         private$session = session;

         output[[private$id]] <- renderCyjShiny(
            cyjShiny(graph=private$graph.json, layoutName="random")
            ) # renderIgvShiny
         printf("--- leaving cyjWidget::server")
          }, # server

        #' @description
        #' scales the entire graph to fit symetrically in its div
        #' @returns nothing
      fitGraph = function(){
         fit(private$session)
         }

     ) # public
   ) # class
#----------------------------------------------------------------------------------------------------

