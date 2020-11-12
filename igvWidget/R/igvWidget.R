#' R6 Class representing an igv.js genome browser
#'
#' @import igvShiny
#' @import rtracklayer
#' @import VariantAnnotation
#'
#' @title igvWidget
#' @description an R6 class providing clean access to the igvShiny package
#' @name igvWidget
#' @details
#' This class provides the igv.js genome browser in a classic object-oriented-programming R context
#'
library(R6)
library(igvShiny)

#' @export
igvWidget = R6Class("igvWidget",

    private = list(
        id = NULL,
        genome = NULL,
        locus = NULL,
        width = NULL,
        height = NULL,
        border = NULL,
        session = NULL,
        currentGenomicRegion = NULL,
        digestTrackClickData = function(x){
           printf("--- clackTrick")
           print(x)
           name.indices <- grep("name", names(x))
           value.indices <- grep("value", names(x))
           printf("=== name indices:")
           print(name.indices)
           printf("=== value indices:")
           print(value.indices)
           entity <- "no entity found"
           if(length(name.indices) == length(value.indices)){
              clickData <- as.character(x[value.indices])
              names(clickData) <- as.character(x[name.indices])
              printf("=== extracted clickData")
              print(clickData)
              nameVariable <- grep("name", names(clickData), ignore.case=TRUE, value=TRUE)
              printf("=== nameVariable")
              print(nameVariable)
              if(nchar(nameVariable) == 4){
                 entity <- clickData[[nameVariable]]
                 printf("you clicked on entity '%s'", entity)
                 } # there is a name field
              } # the data structure returned from javascript has #name = #value fields
           return(entity)
           } # private method digestTrackClickData
        ),

        #' @description
        #' Create a new igvWidget
        #' @param id the html document div id
        #' @param genome character string, eg "hg38", "mm10"
        #' @param locus  character string, a geneSymbol or chrom:start-end
        #' @param width  integer, in width
        #' @param height integer, in pixels
        #' @param border character string, for instance "1px solid purple; border-radius: 5px;"
        #' @return A new `igvWidget` object.

   public = list(

      trackClickResult = NULL,

      initialize = function(id, genome, locus, width="95%", height="500px", border){
          private$id = id;
          private$genome = genome;
          private$locus = locus;
          private$width = width;
          private$height = height;
          private$border = border;
          private$currentGenomicRegion = locus;
          },

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
          tagList(
             div(id=sprintf("%s-container", private$id),
               igvShinyOutput(private$id, private$width, private$height),
               style=sprintf("width: %s; height: %s; border: %s; overflow: scroll; background-color: white;",
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
         printf("--- starting igvWidget$server")
         private$session = session;

            # this public variable is a reactive function, by which clients
            # can respond to igv.js track click events
         self$trackClickResult = reactiveVal("")

         observeEvent(input$trackClick, {
            x <- input$trackClick
            printf("==== track click seen in igvWidget.R")
            print(x)
            printf("==========================================")
            save(x, file="debug-track-click.RData")
            name.indices <- grep("name", names(x))
            value.indices <- grep("value", names(x))
            values <- x[value.indices]
            values <- gsub("&nbsp;", "", values)
            tbl <- data.frame(value=values)
            row.names <- as.character(x[name.indices])
            row.names <- gsub(":", "", row.names)
            row.names <- gsub("&nbsp;", "", row.names)
            row.names <- make.names(row.names, unique=TRUE)
            rownames(tbl) <- row.names[seq_len(length(values))]
            printf("------- tbl.click:");
            print(tbl)
            self$trackClickResult(tbl)
            })

         observeEvent(input[[sprintf("currentGenomicRegion.%s", private$id)]], {
           loc <- input[[sprintf("currentGenomicRegion.%s", private$id)]]
           private$currentGenomicRegion = loc
           })

         output[[private$id]] <- renderIgvShiny(
            igvShiny(options=list(genomeName=private$genome,
                                  initialLocus=private$locus,
                                  displayMode="SQUISHED",
                                  trackHeight=300
                                  ),
                       width=private$width,
                       height=private$height
                       )
               ) # renderIgvShiny
          }, # server

        #' @description
        #' igv.js triggers an event when the track is clicked.  we parse that into an
        #' "entity" as best we can:  a gene, a variant, a track block's chromLoc
        #' @returns character string
      getTrackClickEntity = function(){
         return(private$trackClickEntity)
         },

        #' @description
        #' directs the igv browser to display, and zoom in on, the specified locus
        #' @param newLocus character string, either a recognized geneSymbol or  "chrom:start-end"
        #' @returns nothing


      setLocus = function(newLocus){
          showGenomicRegion(private$session, private$id, newLocus)
          },


        #' @description
        #' queries for the currently visible locus in the genome browser
        #' @returns a "chrom:start-end" string (or, in rare circumstances, a geneSymbol)
      getLocus = function(){
          locusString <- private$currentGenomicRegion
          if(!grepl("^chr", locusString, ignore.case=TRUE, fixed=FALSE))
             return(locusString)
          printf("=== getLocus, locusString: ")
          print(locusString)
          x = parseChromLocString(locusString)
          return(x)
          },

        #' @description
        #' displays a BED track in the genome browser
        #' @param trackName character string, e.g., "ATAC-seq hour 2"
        #' @param tbl.bed data.frame with 3 or more columns, typically chrom, start, end, name
        #' @param color character string, a standard CSS color name or RGB triple ("#FFAA00") or "random"
        #' @param trackHeight integer height in pixels in which to display the track
        #' @param deleteTracksOfSameName logical almost always TRUE; FALSE allows multiple tracks with the same name
        #' @param quiet logical shows trace of multiple steps along the way
        #' @returns nothing
      displayBedTrack = function(trackName, tbl.bed, color, trackHeight,
                                 deleteTracksOfSameName=TRUE, quiet=TRUE){
          loadBedTrack(private$session, private$id,
                       trackName, tbl.bed, color, trackHeight=trackHeight, deleteTracksOfSameName, quiet)
          },

        #' @description
        #' displays a bedGraph track in the genome browser, uses a numeric scale, score mapped to height
        #' @param trackName character string, e.g., "ATAC-seq hour 2"
        #' @param tbl.bg data.frame with 4 or more columns: chrom, start, end, score
        #' @param color character string, a standard CSS color name or RGB triple ("#FFAA00") or "random"
        #' @param trackHeight integer height in pixels in which to display the track
        #' @param autoscale logical, track score scale calculated from data, or not
        #' @param min numeric explicit bound on the track score scale
        #' @param max numeric explicit bound on the track score scale
        #' @param deleteTracksOfSameName logical almost always TRUE; FALSE allows multiple tracks with the same name
        #' @param quiet logical shows trace of multiple steps along the way
        #' @returns nothing

      displayBedGraphTrack = function(trackName, tbl.bg, color, trackHeight,
                                      autoscale, min=NA_real_, max=NA_real_,
                                 deleteTracksOfSameName=TRUE, quiet=TRUE){
          printf("=== igvWidget, displayBedGraphTrack")
          print(head(tbl.bg))
          printf("==================")
          print(lapply(tbl.bg, class))
          colnames(tbl.bg)[4] <- "score"
          loadBedGraphTrack(private$session, private$id,
                            trackName, tbl.bg, color, autoscale, min, max, trackHeight=trackHeight,
                            deleteTracksOfSameName, quiet)
          },

        #' @description
        #' displays a VCF (Variant Call Format) track in the genome browser
        #' @param trackName character string, e.g., "ATAC-seq hour 2"
        #' @param vcfData a largely opaque object read by the VariantAnnotation package from a vcf file
        #' @param deleteTracksOfSameName logical almost always TRUE; FALSE allows multiple tracks with the same name
        #' @returns nothing

      displayVcfTrack = function(trackName, vcfData, deleteTracksOfSameName=TRUE){
         loadVcfTrack(private$session, private$id, trackName, vcfData, deleteTracksOfSameName)
         },

        #' @description
        #' displays a GWAS (genome-wide association study) track in the genome browser
        #' @param trackName character string, e.g., "ATAC-seq hour 2"
        #' @param tbl.gwas data.frame, see details below
        #' @param deleteTracksOfSameName logical almost always TRUE; FALSE allows multiple tracks with the same name
        #' @details the GWAS file format has these columns; two example lines follow
        #'               SNPS      cptid CHR_ID  CHR_POS EA NEA  EAF    Beta     SE   P.VALUE Effective_N
        #'  14517  rs79350681  1:5330646      1  5330646  a   g 0.02 -0.7356 0.1875 8.770e-05        2902
        #'  14527 rs187641513  1:5331868      1  5331868  a   g 0.02 -0.7424 0.1889 8.493e-05        2902
        #'
        #' @returns nothing
      displayGwasTrack = function(trackName, tbl.gwas, deleteTracksOfSameName=TRUE){
          printf("=== igvWidget$displayGwasTrack %s, %d rows", trackName, nrow(tbl.gwas))
          loadGwasTrack(private$session, private$id, trackName, tbl.gwas, deleteTracksOfSameName)
          },

      # (15 sep 2020: fails in igv.min.js: zv.getChromosomeName, cannot read toLowerCase of undefined
      displaySegTrack = function(trackName, tbl, deleteTracksOfSameName=TRUE){
         loadSegTrack(private$session, private$id, trackName, tbl, deleteTracksOfSameName)
         }

     ) # public
  ) # class

