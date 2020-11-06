#' R6 Class representing a simple text display widget
#'
#' @import R6
#' @import shiny
#' @import shinyWidgets
#' @import dataTableWidget
#' @import GOstats
#' @import GO.db
#' @import org.Hs.eg.db
#' @import shinybusy
#'
#'
#' @title GOEnrichmentWidget
#' @name GOEnrichmentWidget
#'
#' @description an R6 class providing clean access to Gene Ontology Enrichment, input & display
#'
#------------------------------------------------------------------------------------------------------------------------
printf <- function(...) print(noquote(sprintf(...)))
#------------------------------------------------------------------------------------------------------------------------
assignGeneIDs <- function(symbols)
{
    #printf("entering assignGeneIDs, org is of class '%s", class(org.Hs.egSYMBOL2EG))
    #print(org.Hs.egSYMBOL2EG)
    #geneIDs <- mget(symbols, org.Hs.egSYMBOL2EG, ifnotfound=NA)
    geneIDs <-AnnotationDbi::select(org.Hs.eg.db, symbols, keytype="SYMBOL",
                                    columns=c("SYMBOL", "ENTREZID"))$ENTREZID

    if(all(!is.na(geneIDs)) & all(sapply(geneIDs, length) == 1))
        return(list(mapped=geneIDs, failures=NULL, multiples=NULL))

    multiples <- names(which(sapply(geneIDs, length) > 1))

    if(length(multiples) == 0)
        multiples <- NULL

    unmapped <- names(which(is.na(geneIDs)))


    if(length(unmapped) == 0){
        unmapped <- NULL
        }
    else{
        aliasIDs <- mget(unmapped, org.Hs.egALIAS2EG, ifnotfound=NA)
        unmapped <- names(which(is.na(aliasIDs)))
        #browser("which")
        new.multiples <- names(which(sapply(aliasIDs, length) > 1))
        if(length(new.multiples) > 0)
            multiples <- c(multiples, new.multiples)
        #browser("assign")
        geneIDs[names(aliasIDs)] <- aliasIDs
        }

    if(length(unmapped) == 0)
        unmapped <- NULL
    if(all(is.na(geneIDs)))
        geneIDs <- NULL
    if(length(multiples) == 0)
        multiples <- NULL

    return(list(mapped=geneIDs, failures=unmapped, multiples=multiples))

} # assignGeneIDs
#------------------------------------------------------------------------------------------------------------------------
goEnrich <- function(geneSymbols, maxRows=10, paste.character=",")
{
  t0 <- Sys.time()

  suppressMessages(
     symbol.entrez.map <- assignGeneIDs(geneSymbols)
     )

  gene.universe = character(0)
  geneIDs <- unlist(symbol.entrez.map$mapped, use.names=FALSE)

  tbl.map <- data.frame()
  if(length(geneIDs) == length(geneSymbols))
    tbl.map <- data.frame(id=geneIDs, sym=geneSymbols, stringsAsFactors=FALSE)

  go.params <- new("GOHyperGParams", geneIds=unique(tbl.map$id),
                   universeGeneIds = gene.universe, annotation = "org.Hs.eg.db",
                   ontology = 'BP', pvalueCutoff = 0.05, conditional = FALSE,
                   testDirection = "over")

  go.bp.hgr <- hyperGTest(go.params)
  tbl.go <- summary(go.bp.hgr)
  t1 <- Sys.time()
  # printf("hyperGTest: %s", t1 - t0)

  if(nrow(tbl.go) > maxRows)
     tbl.go <- head(tbl.go, n=maxRows)   # handles the -1 case (all rows) implicitly

  suppressMessages(
     tbl.withSyms <- AnnotationDbi::select(org.Hs.eg.db, keys=tbl.go$GOBPID, keytype="GOALL",columns=c("GOALL", "ENTREZID", "SYMBOL"))
     )
  tbl.withSyms <- subset(tbl.withSyms, SYMBOL %in% geneSymbols)
  tbl.withSyms <- unique(tbl.withSyms[, c("GOALL", "SYMBOL")])
  symbol.annotation <- unlist(lapply(tbl.go$GOBPID,
                                     function(goTerm)
                                         paste(subset(tbl.withSyms, GOALL==goTerm)$SYMBOL,
                                               collapse=paste.character)))
  tbl.go$genes <- symbol.annotation
  t2 <- Sys.time()

  #printf("gene assignment: %s", t2 - t1)

  return(tbl.go)

} # goEnrich
#------------------------------------------------------------------------------------------------------------------------
test_goEnrich <- function()
{
   igap.ad.genes <- c("CR1", "BIN1", "CD2AP", "EPHA1", "CLU", "MS4A6A", "PICALM",
                      "ABCA7", "CD33", "HLA-DRB5", "HLA-DRB1", "PTK2B", "SORL1",
                      "SLC24A4", "RIN3", "DSG2", "INPP5D", "MEF2C", "NME8", "ZCWPW1",
                      "CELF1", "FERMT2", "CASS4", "APOE", "TOMM40")

   tbl.go <- goEnrich(head(igap.ad.genes, n=-1), maxRows=10)
   dim(tbl.go)
   checkEquals(ncol(tbl.go), 8)
   checkTrue(nrow(tbl.go) > 100)
   checkEquals(tbl.go$Term[1], "negative regulation of amyloid-beta formation")
   checkEquals(tbl.go$genes[1], "BIN1;CLU")


} # test_goEnrich
#--------------------------------------------------------------------------------
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
          private$dtw = dataTableWidget$new(id="goEnrichmentDT",
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
         }, # server
      setGenes = function(genes){
         private$geneSymbols <- genes
         },

      run = function(genes){
         private$geneSymbols <- genes
         show_modal_spinner(spin="dots", text="GO enrichment calculation takes about a minute.")
         tbl.go <- goEnrich(genes, maxRows=-1, paste.character=" ")
         tbl.go$Pvalue <- format(tbl.go$Pvalue, digits=3)
         coi <- c("genes", "Term", "Count", "Size", "Pvalue")
         remove_modal_spinner()
         private$dtw$setTable(tbl.go[, coi])
         }

     ) # public
  ) # class
#------------------------------------------------------------------------------------------------------------------------

