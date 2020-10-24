#' R6 Class encapsulating a tms (TrenaMultiScore) table with filter controls
#'
#' @import dataTableWidget
#' @import shinyWidgets
#' @import R6
#'
#' @description encapsulating a tms (TrenaMultiScore) table with filter controls
#' @name tmsTableWidget
#'
printf <- function(...) print(noquote(sprintf(...)))

#' @export
tmsTableWidget = R6Class("tmsTableWidget",

    private = list(
        id = NULL,
        title = NULL,
        tbl = NULL,
        dtw = NULL,
        session = NULL,
        input = NULL,
        output = NULL
        ),

        #' @description
        #' Create a new tmsTableWidget
        #' @param id the html document div id
        #' @param title character
        #' @param tbl data.frame, TrenaMultiScore output
        #' @return A new `tmsTableWidget` object.

   public = list(

       initialize = function(id, title, tbl){

          printf("entering tmsTableWidget::initialize")
          private$id = id;
          private$title = title;
          private$tbl = tbl;
          private$dtw = dataTableWidget$new(id="dt",
                                            private$tbl,
                                            pageLength=15,
                                            lengthMenu=c(4, 10, 15, 20, 25, 30, 50),
                                            width="1600px", height="1000px")
          }, # initialize

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
              sidebarLayout(
                  sidebarPanel(
                   pickerInput(inputId = "tfPicker",
                                label = "Filter on TF",
                                choices = sort(unique(private$tbl$tf)),
                                options = list(`actions-box`=TRUE, size=20,
                                               `selected-text-format`="count > 6"),
                                multiple = TRUE),
                   sliderInput("absDiffbind", "abs(diffbind):", min=0, max=4.0, value=c(0.0, 4.0), step=0.1),
                   sliderInput("absCorrelation", "abs(cor):", min=0, max=1.0, value=c(0.0, 1.0)),
                   sliderInput("absTSS", "log(abs(tss)):", min=0, max=10, value = c(0, 10), step=0.2),
                   sliderInput("motifScore", "motif score:", min=0, max=10, value=c(3, 10), step=0.5),
                   sliderInput("geneHancer", "GeneHancer combined score:", min = 0, max = 700,
                               value = c(0, 700)),
                   radioButtons(
                      inputId="chipRadioButtons",
                      label="ChIP",
                      choices = c("yes", "no", "either"),
                      selected = "either",
                      inline = TRUE,
                      width = NULL,
                      choiceNames = NULL,
                      choiceValues = NULL),
                      width=2), # sidebarPanel
               mainPanel(
                   private$dtw$ui(),
                   width=10
                 )
              ) # sidebarLayout
          }, # ui

        #' @description
        #' defines the event handlers used with the ui
        #' @param input list, managed by Shiny
        #' @param output  list, managed by Shiny
        #' @param session  list (environment?) managed by Shiny
        #' @returns nothing
      server = function(input, output, session){
         printf("--- executing tmsTableWidget::server")
         private$session = session;
         private$input = input
         private$output = output
         private$dtw$server(input, output, session)
         observe({
             printf("--- filtering triggered")
             tfs <- input$tfPicker

             if(is.null(tfs))
                 tbl.sub <- private$tbl
             else
                 tbl.sub <- subset(private$tbl, tf %in% tfs)

             chipChoice <- input$chipRadioButtons
             tbl.sub <- switch(chipChoice,
                               "yes"    = subset(tbl.sub, chip),
                               "no"     = subset(tbl.sub, !chip),
                               "either" = tbl.sub)

             absDiffbind <- input$absDiffbind
             tbl.sub <- subset(tbl.sub, abs(diffbind.score) >= absDiffbind[1] &
                                        abs(diffbind.score) <= absDiffbind[2])

             absCorrelation <- input$absCorrelation
             tbl.sub <- subset(tbl.sub, abs(cor) >= absCorrelation[1] & abs(cor) <= absCorrelation[2])

             absTSS <- input$absTSS
             lowerBound <- 10^(absTSS[1])
             upperBound <- 10^(absTSS[2])
             printf("upperBound: %f    lowerBound: %f", lowerBound, upperBound)
             tbl.sub <- subset(tbl.sub, abs(tss) >= lowerBound & abs(tss) <= upperBound)

             motifScores <- input$motifScore
             tbl.sub <- subset(tbl.sub, motifScore >= motifScores[1] & motifScore <= motifScores[2])

             geneHancer <- input$geneHancer
             tbl.sub <- subset(tbl.sub, gh >= geneHancer[1] & gh <= geneHancer[2])

             printf("--- observe, about to setTable of %d rows", nrow(tbl.sub))
             private$dtw$setTable(tbl.sub)
             })

         } # server

     ) # public
  ) # class

