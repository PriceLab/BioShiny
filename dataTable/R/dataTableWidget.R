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
        width = NULL,
        height = NULL,
        border = NULL,
        pageLength = NULL,
        selectionPolicy = NULL,
        wrapLongTextInCells = NULL,
        searchString = NULL,
        rownames.to.display = NULL,
        searchOptions = NULL,
        DTclass = NULL,
        selectionOption = NULL,
        dataTableProxy = NULL
        ),

        #' @description
        #' Create a new dataTableWidget
        #' @param id the html document div id
        #' @param tbl data.frame
        #' @param width integer, 800 by default
        #' @param height integer, 800 by default
        #' @param border string, "0px;" by default
        #' @param pageLength integer, 10 by default
        #' @return A new `dataTableWidget` object.

   public = list(

       tableSelection = NULL,   # a reactive value, see server() below

       initialize = function(id, tbl, width, height, border="0px;", pageLength=10){
          printf("entering dataTableWidget::initialize")
          private$id = id;
          private$tbl = tbl;

          private$width = width;
          private$height = height;
          private$border = border;

          private$pageLength = pageLength
          private$searchOptions = list();
          private$DTclass = "display"
          private$selectionOption = list(mode="multiple", selected=NULL)
          },

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
         tagList(
            div(
               DT::DTOutput(private$id),
               style=sprintf("width: %dpx; height: %dpx; padding: 20px; border: %s; overflow: auto; background-color: white;",
                             private$width+50, private$height+125, private$border))
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
         observeEvent(input[[sprintf("%s_rows_selected", private$id)]], ignoreInit=TRUE, ignoreNULL=FALSE, {
            row.numbers <- input[[sprintf("%s_rows_selected", private$id)]]
            #print("--- widget row selection made")
            #print(row.numbers)
            if(all(is.null(row.numbers)))
               newValue <- integer(0)
            else
               newValue <- rownames(private$tbl)[row.numbers]
            #printf("returning these row names: %s", paste(newValue, collapse=","))
            self$tableSelection(newValue)
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
                                         pageLength = private$pageLength,
                                         paging=TRUE),
                            selection=private$selectionOption)
          })  # renderDataTable
         private$dataTableProxy <- dataTableProxy(private$id)
         }, # setTable

      selectRows = function(rowNumbers){
         printf("DTW::selectRows: %s", paste(rowNumbers, collapse=","))
         selectRows(private$dataTableProxy, rowNumbers, ignore.selectable = TRUE)
         }, # selectRows

      clearSelection = function(){
         selectRows(private$dataTableProxy, NULL)
         } # selectRows

     ) # public
#----------------------------------------------------------------------------------------------------
) # DataTableWidget class


