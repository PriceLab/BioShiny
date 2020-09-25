#' R6 Class representing a simple text display widget
#'
#' @title dataTableWidget
#' @description an R6 class providing clean access to a simple html message box
#' @name dataTableWidget
#'
#' @import R6
#' @import DT
#' @import shiny
#'
#' @export
dataTableWidget = R6Class("dataTableWidget",

    private = list(
        id = NULL,
        input = NULL,
        output = NULL,
        session = NULL,
        tbl = NULL,
        selectionPolicy = NULL,
        wrapLongTextInCells = NULL,
        searchString = NULL,
        rownames.to.display = NULL,
        searchOptions = NULL,
        DTclass = NULL,
        selectionOption = NULL
        ),

        #' @description
        #' Create a new dataTableWidget
        #' @param id the html document div id
        #' @param title character
        #' @param boxWidth integer, 200 by default
        #' @param boxHeight integer, 30 by default
        #' @param fontSize integer, 20 by default
        #' @param fontColor character, standard CSS rgb or name
        #' @param backgroundColor character, standard CSS rgb or name
        #' @return A new `dataTableWidget` object.

   public = list(

       tableSelection = NULL,   # a reactive value, see server() below

       initialize = function(id, tbl){
          printf("entering dataTableWidget::initialize")
          private$id = id;
          private$tbl = tbl;
          private$searchOptions = list();
          private$DTclass = "display"
          private$selectionOption = list(selected=NULL)
          },

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
          tagList(
             DT::DTOutput(private$id)
             )
          }, # ui

        #' @description
        #' defines the event handlers used with the ui
        #' @param input list, managed by Shiny
        #' @param output  list, managed by Shiny
        #' @param session  list (environment?) managed by Shiny
        #' @returns nothing
      server = function(input, output, session){

         private$session = session;
         private$input = input
         private$output = output
         if(!is.null(private$tbl)) self$setTable(private$tbl)

         self$tableSelection = reactiveVal()
         observeEvent(input[[sprintf("%s_rows_selected", private$id)]], ignoreInit=TRUE, {
           row.numbers <- input[[sprintf("%s_rows_selected", private$id)]]
           print("--- widget row selection made")
           print(row.numbers)
           self$tableSelection(row.numbers)
           })

         }, # server

      setTable = function(tbl){
          private$output[[private$id]] <- DT::renderDataTable({
              DT::datatable(tbl,
                            rownames=TRUE,
                            class=private$DTclass,
                            options=list(dom='<lfip<t>>',
                                         scrollX=TRUE,
                                         search=private$searchOptions,
                                         lengthMenu = c(3,5,10,50),
                                         pageLength = 5,
                                         paging=TRUE),
                            selection=private$selectionOption)
          })  # renderDataTable
         } # setTable

     ) # public
#----------------------------------------------------------------------------------------------------
) # DataTableWidget class


