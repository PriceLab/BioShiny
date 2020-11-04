#' R6 Class representing a simple text display widget
#'
#' @import R6
#' @import shiny
#' @import shinyWidgets
#' @import dataTableWidget
## @import GSEABase
#' @import GOstats
#' @import GO.db
## @import Category
#' @import org.Hs.eg.db
#' @import shinybusy
#'
#'
#' @title GOEnrichmentWidget
#' @name GOEnrichmentWidget
#'
#' @description an R6 class providing clean access to Gene Ontology Enrichment, input & display
#'
#library(org.Hs.eg.db)
#------------------------------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))

#' @export
GOEnrichmentWidget = R6Class("GOEnrichmentWidget",

    private = list(
        id = NULL,
        title = NULL,
        geneSymbols = NULL,
        tbl = NULL,
        dtw = NULL,
        session = NULL,
        input = NULL,
        output = NULL
        ),

        #' @description
        #' Create a new GOEnrichmentWidget
        #' @param id the html document div id
        #' @param title character
        #' @param geneSymbols character vector, HUGO standard names only
        #
        #' @return A new `GOEnrichmentWidget` object.
        #' @export

   public = list(

       initialize = function(id, title, geneSymbols){

          printf("entering GOEnrichmentWidget::initialize")
          private$id = id;
          private$title = title;
          private$geneSymbols = geneSymbols;
          private$tbl <- data.frame()
          private$dtw = dataTableWidget$new(id="dt",
                                            private$tbl,
                                            pageLength=20,
                                            lengthMenu=c(4, 10, 15, 20, 25, 30, 50),
                                            width="1600px", height="1000px")

          },

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
          fluidPage(
              titlePanel("Gene Ontology Enrichment for Biological Process"),
                sidebarLayout(
                     sidebarPanel(
                         pickerInput(inputId = "genePicker",
                           label = "Genes",
                           choices = sort(unique(private$geneSymbols)),
                           options = list(`actions-box`=TRUE, size=20,
                                          `selected-text-format`="count > 4"),
                            multiple = TRUE),
                         actionButton("calculateEnrichmentButton", "Calculate"),
                         width=3
                        ), # sidebarPanel
            mainPanel(
                div(
                  private$dtw$ui(),
                  style="width: 1000px; height: 1000px; margin: 10px; padding: 10px; overflow-x: scroll; overflow-y: scroll;"),
               width=9
               )
              ) # sidebarLayout
            ) # fluidPage
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
         private$dtw$server(input, output, session)

         observeEvent(input$calculateEnrichmentButton, ignoreInit=TRUE, {
            printf("--- calculate GO enrichment")
            goi <- private$geneSymbols
            selected.genes <- input$genePicker
            if(!all(is.null(selected.genes)))
               goi <- selected.genes
            show_modal_spinner(spin="dots", text="GO enrichment calculation takes about a minute.")
            tbl.go <- goEnrich(goi, maxRows=-1, paste.character=" ")
            tbl.go$Pvalue <- format(tbl.go$Pvalue, digits=3)
            coi <- c("genes", "Term", "Count", "Size", "Pvalue")
            remove_modal_spinner()
            private$dtw$setTable(tbl.go[, coi])
            })
         } # server

     ) # public
  ) # class

