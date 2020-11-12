#' R6 Class representing one or more igv genome tracks, from tf/targetGene data
#'
#' @import R6
#' @import shiny
#' @import shinyWidgets
#' @import igvWidget
#' @import shinybusy
#' @import later
#'
#'
#' @title GenomeTracksWidget
#' @name GenomeTracksWidget
#'
#' @description an R6 class providing clean access to Gene Ontology Enrichment, input & display
#'
#------------------------------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
#' @export
GenomeTracksWidget = R6Class("GenomeTracksWidget",

    private = list(
        id = NULL,
        title = NULL,
        tbl = NULL,
        targetGenes = NULL,
        trackList = NULL,
        session = NULL,
        input = NULL,
        output = NULL
        ),

        #' @description
        #' Create a new GenomeTracksWidget
        #' @param id the html document div id
        #' @param title character
        #' @param geneSymbols character vector, HUGO standard names only
        #
        #' @return A new `GenomeTracksWidget` object.
        #' @export

   public = list(

       initialize = function(id, title) {

          printf("entering GenomeTracksWidget::initialize")
          private$id = id;
          private$title = title;
          private$tbl <- data.frame()
          private$targetGenes = c()
          private$trackList <- list()
          },

      setTable = function(tbl){
         private$tbl <- tbl
         printf("dim(tbl): %d %d", nrow(tbl), ncol(tbl))
         private$targetGenes <- sort(unique(tbl$targetGene))
         },

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
          fluidPage(
                sidebarLayout(
                     sidebarPanel(
                         pickerInput(inputId = "genomeTrackGenePicker",
                                     label = "Target Genes",
                                     choices = private$targetGenes,
                                     multiple = FALSE),
                         actionButton("addTrackButton", "Display Track"),
                         #br(), br(), br(),
                         #pickerInput(inputId = "trackDestroyer",
                         #            label = "Remove Track",
                         #            choices = "",
                         #            multiple = FALSE),
                         #actionButton("removeTrackButton", "Remove Track"),
                         width=2
                        ), # sidebarPanel
                    mainPanel(
                        tabsetPanel(id="igvTabs"),
                                    #tabPanel("Hello", "This is the hello tab")),
                       width=10
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

         observeEvent(input$addTrackButton, ignoreInit=TRUE, {
            targetGene <- isolate(input$genomeTrackGenePicker)
            #igvWidgetID <- sprintf("igv-%s-%d", targetGene, sample(1:1000000, size=1))
            igvWidgetID <- sprintf("igv-%s", targetGene)
            printf("--- GenomeTracksWidget creating new igv with id %s", igvWidgetID)
            newTrackWidget <- igvWidget$new(igvWidgetID,
                                            genome="hg38",
                                            locus=targetGene,
                                            width="100%",
                                            height="500",
                                            border="")
            private$trackList[[targetGene]] <- newTrackWidget
            removeTab(inputId="igvTabs", target=targetGene)
            insertTab(inputId="igvTabs", position="after",
                      tabPanel(targetGene, newTrackWidget$ui()),
                      select=TRUE, target=NULL)
            newTrackWidget$server(input, output, session)
            later(function(){
                self$addAllRelevantTracks(targetGene, newTrackWidget, trackHeight=30)}, 3)
            #insertUI(selector="#tracksContainer", where="beforeEnd",
            #         newTrackWidget$ui(), immediate=TRUE)
            #newTrackWidget$server(input, output, session)
            # updatePickerInput(session, inputId="trackDestroyer",
            #                  choices=c(names(private$trackList)))
            #printf("--- about to call addAllRelevantTracks")
            }) # addTrackButton

         observeEvent(input$removeTrackButton, ignoreInit=TRUE, {
            targetTrackName <- isolate(input$trackDestroyer)
            searchTerm <- sprintf("^%s$", targetTrackName)
            index <- grep(searchTerm, names(private$trackList))
            if(length(index) == 1)
                private$trackList <- private$trackList[-index]
            printf("--- remove track %s", targetTrackName)

            trackID <- sprintf("#igv-%s", targetTrackName)
            trackContainerID <- sprintf("#igv-%s-container", targetTrackName)
            trackContainerID.withRootClass <- sprintf("%s .igv-root-div", trackID)
            removeUI(selector=trackContainerID.withRootClass, immediate=TRUE)
            removeUI(selector=trackID, immediate=TRUE)
            removeUI(selector=trackContainerID, immediate=TRUE)
            #removeUI(selector=trackID, immediate=TRUE)
            }) # removeTrackButton


         }, # server

      addAllRelevantTracks = function(gene, newTrackWidget, trackHeight){
         tbl.oc <- subset(private$tbl, targetGene==gene)[, c("chrom", "start", "end", "tf")]
         current.locus <- newTrackWidget$getLocus()
         printf("--- current.locus")
         print(current.locus)
         shoulder <- 5000
         trackMin <- min(tbl.oc$start) - shoulder
         trackMax <- max(tbl.oc$end) + shoulder
         overall.min <- min(current.locus$start, trackMin)
         overall.max <- max(current.locus$end,   trackMax)
         newTrackWidget$setLocus(sprintf("%s:%d-%d", tbl.oc$chrom[1], overall.min, overall.max))
         printf("oc for %s: %d", gene, nrow(tbl.oc))
         tfs <- sort(unique(tbl.oc$tf))
         for(this.tf in tfs){
             newTrackWidget$displayBedTrack(this.tf, subset(tbl.oc, tf==this.tf), color="random",
                                            trackHeight=trackHeight)
            } # for tf
         } # addAllRelevantTracks

     ) # public




  ) # class
#------------------------------------------------------------------------------------------------------------------------

