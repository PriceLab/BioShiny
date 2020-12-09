printf <- function(...) print(noquote(sprintf(...)))

#' R6 Class representing a simple text display widget
#'
#' @title dataTableWidget
#' @description an R6 class providing clean access to a simple html message box.
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
        pixel.width = NULL,
        pixel.height = NULL,
        border = NULL,
        pageLength = NULL,
        lengthMenu = NULL,
        columnWidths = NULL,
        selectionPolicy = NULL,      # single, multiple, none
        selectionTarget = NULL,      # row, column, cell
        wrapLongTextInCells = NULL,
        searchString = NULL,
        rownames.to.display = NULL,
        searchOptions = NULL,
        DTclass = NULL,
        selectionOption = NULL,
        dataTableProxy = NULL,
        parseSizeString = function(x){
            if(is.numeric(x)) return(x);
            if(grepl("px", x))
               return(as.numeric(sub("px", "", x)))
            if(grepl("%", x))
               return(900)
            } # parseSizeString
        ),

        #' @description
        #' Create a new dataTableWidget.
        #'
        #' @param id the html document div id
        #' @param tbl data.frame
        #' @param width character, "800px;" by default.  "100%;" can be useful.
        #' @param height integer, "600px;" by default, percentage heights do not work.
        #' @param border string, "0px;" by default, "1px blue solid; border-radius: 5px;" useful.
        #' @param pageLength integer, 10 by default
        #' @param wrapLines logical, FALSE by default, gives more rows on screen
        #' @param selectionMode character, "single", "none", or "multiple", "single" by default
        #' @param selectionTarget character, "row" or "column" or "cell"
        #' @param lengthMenu integers specifies page length options (# of rows onscreen). default (5,10,25,50)
        #' @param columnWidths integer vector, widths in pixels, NULL default, applies all
        #'        supplied, default for the remainder
        #' @return A new `dataTableWidget` object.

   public = list(

       tableSelection = NULL,   # a reactive value, see server() below

       initialize = function(id, tbl, width="95%", height="500px", border="0px;", pageLength=10,
                             wrapLines=FALSE,
                             selectionMode="single",
                             selectionTarget="row",
                             lengthMenu=c(5,10,25,50),
                             columnWidths=NULL){
          printf("entering dataTableWidget::initialize")
          private$id = id;
          private$tbl = tbl;

          private$width = width;
          private$height = height;
          private$pixel.width = private$parseSizeString(width);
          private$pixel.height = private$parseSizeString(height);

          private$border = border;
          printf("dtw dimensions: %s x %s", private$width, private$height)

          private$pageLength = pageLength
          private$lengthMenu = lengthMenu
          private$columnWidths = columnWidths
          private$searchOptions = list();
          private$DTclass = "display"
          if(!wrapLines)
              private$DTclass = paste0(private$DTclass, " nowrap")
          private$selectionOption = list(mode=selectionMode, target=selectionTarget, selected=NULL)
          },

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
         tagList(
            #div(
               DT::DTOutput(private$id)
               #style=sprintf("width: %s; height: %s; padding: 10px; margin: 3px; border: %s; overflow: auto; background-color: white;",
               #              private$width, private$height, private$border))
             )
          }, # ui

        #' @description
        #' defines the event handlers used with the ui
        #'
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
               newValue <- row.numbers
            #printf("returning these row names: %s", paste(newValue, collapse=","))
            self$tableSelection(newValue)
            })

         observeEvent(input[[sprintf("%s_cells_selected", private$id)]], ignoreInit=TRUE, ignoreNULL=FALSE, {
            cell.numbers <- input[[sprintf("%s_cells_selected", private$id)]]
            if(all(is.null(cell.numbers)))
               newValue <- integer(0)
            else
               newValue <- cell.numbers
            #printf("returning these row names: %s", paste(newValue, collapse=","))
            self$tableSelection(newValue)
            })

         observeEvent(input[[sprintf("%s_columns_selected", private$id)]], ignoreInit=TRUE, ignoreNULL=FALSE, {
            column.numbers <- input[[sprintf("%s_columns_selected", private$id)]]
            if(all(is.null(column.numbers)))
               newValue <- integer(0)
            else
               newValue <- column.numbers
            self$tableSelection(newValue)
            })

         }, # server

        #' @description
        #' installs a new data.frame.
        #'
        #' @param tbl a data.frame
        #' @returns nothing
      setTable = function(tbl){
          widths <- private$columnWidths
          column.widths <- list() # add values, or leave empty.  DT accepts both
          if(!is.null(widths[1])){
            for(i in seq_len(length(widths)))
               column.widths[[length(column.widths)+1]] <- list(width=widths[i], targets=i)
            } # is columnWidths provided
          #printf("--- columnWidths")
          #print(column.widths)
          private$output[[private$id]] <- DT::renderDataTable({
              DT::datatable(tbl,
                            rownames=TRUE,
                            class=private$DTclass,
                            options=list(dom='<lfip<t>>',
                                         scrollX=TRUE,
                                         search=private$searchOptions,
                                         lengthMenu = private$lengthMenu,
                                         pageLength = private$pageLength,
                                         autoWidth=FALSE,
                                         #columnDefs = list(list(className='dt-center', targets= "_all")),
                                         columnDefs = column.widths,
                                         paging=TRUE),
                            selection=private$selectionOption)
          })  # renderDataTable
         private$dataTableProxy <- dataTableProxy(private$id)
         }, # setTable

        #' @description
        #' highlight the rows, described by their ordingal number.
        #'
        #' @param rowNumbers an integer vector
        #' @returns nothing
      selectRows = function(rowNumbers){
         printf("DTW::selectRows: %s", paste(rowNumbers, collapse=","))
         selectRows(private$dataTableProxy, rowNumbers, ignore.selectable = TRUE)
         }, # selectRows

        #' @description
        #' clears the current selection, if any
        #'
        #' @returns nothing
      clearSelection = function(){
         selectRows(private$dataTableProxy, NULL)
         }, # selectRows

        #' @description
        #' select columns in the table.
        #' @param colnumbers numeric vector, which columns to select
        #' @returns nothing
      selectColumns = function(colNumbers){
         printf("dataTableWidget::selectColumns")
         selectColumns(private$dataTableProxy, 1:3, ignore.selectable=TRUE)
         }, # selectRows

# @description
        #' display each cell in each row as a single line of text, or multiple
        #'
        #' @param wrapLines logical, TRUE or FALSE
        #' @returns nothing
      setWrapLinesOption = function(wrapLines){
         private$DTclass = "display"
         if(!wrapLines)
            private$DTclass = paste0(private$DTclass, " nowrap")
         self$setTable(private$tbl)
         } # setWrapLinesOption

     ) # public
#----------------------------------------------------------------------------------------------------
) # DataTableWidget class


