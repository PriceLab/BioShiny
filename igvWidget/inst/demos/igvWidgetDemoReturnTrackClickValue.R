library(R6)
library(shiny)
library(shinyBS)
library(VariantAnnotation)
library(igvWidget)
library(msgBoxWidget)
#----------------------------------------------------------------------------------------------------
addResourcePath("tracks", "tracks")
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

igvDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(current.igv.instance = NULL,
                   currentGenomicRegion = NULL,
                   loci.miscellany = c("APOE", "TREM2", "chr19:45,080,628-45,160,149",
                                       "BCL3", "EYS",   "chr6:108,330,780-108,891,282",
                                       "SLC10A6", "BCYRN1", "RXRB",
                                       "OR7E37P", "ECHDC2", "SCML1",
                                       "SNHG16", "MIR4723", "MRPS16",
                                       "IL32", "CAP2", "CYP2R1", "PTPN20")),

    #--------------------------------------------------------------------------------
    public = list(
        igv = NULL,
        msgBox = NULL,
        clickData = NULL,
        #------------------------------------------------------------
        initialize = function(){
            self$igv = igvWidget$new("igv01",
                                         genome="hg38",
                                         locus="APOE",
                                         width="100%",
                                         height="800",
                                         border="1px solid purple; border-radius: 5px;")
            private$current.igv.instance <- self$igv
            self$msgBox = msgBoxWidget$new(id="box1", title="msg")
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              wellPanel(style="width: 1000px;",
                fluidRow(
                  column(2, actionButton("newLocusButton", label="Random Locus", style=buttonStyle)),
                  column(2, actionButton("getLocusButton", label="Get Locus", style=buttonStyle)),
                  column(1, actionButton("loadBedTrackButton", label="Bed", style=buttonStyle)),
                  column(1, actionButton("loadBedGraphTrackButton", label="Bedgraph", style=buttonStyle)),
                  column(1, actionButton("loadVcfTrackButton", label="VCF", style=buttonStyle)),
                  #column(1, actionButton("loadSegTrackButton", label="SEG", style=buttonStyle)),
                  column(1, actionButton("loadGwasTrackButton", label="GWAS", style=buttonStyle)),
                  column(1, actionButton("popupButton", label="Popup", style=buttonStyle))
                  )),
               div(self$msgBox$ui(), style="margin: 10px;"),
               self$igv$ui()
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            self$igv$server(input, output, session)

            observe({
                x <- self$igv$trackClickResult();
                printf("igvWdgetDemoReturnTrackClickValue sees click")
                print(x)
                printf("clickData class: %s", class(x))
                printf("         length: %d", length(x))
                save(x, file="~/tmp/click.RData")
                self$clickData <- x
                showModal(modalDialog(renderTable(x, rownames=TRUE), title="track click"))
                })

            observeEvent(input$newLocusButton, {
               index = sample(seq_len(length(private$loci.miscellany)), 1)
               newLocus = private$loci.miscellany[index]
               private$current.igv.instance$setLocus(newLocus)
               })

            observeEvent(input$getLocusButton, {
               printf("--- locus button event")
               showModal(modalDialog(private$current.igv.instance$getLocus(),
                                     size=c("s", "m", "l")[1], title="Locus", easyClose=TRUE))
               print(private$current.igv.instance$getLocus())
               })

            observeEvent(input$loadBedTrackButton, {
               printf("--- load bed track")
               private$current.igv.instance$setLocus("APOE") # 44,904,796-44,910,393"
               tbl.bed <- data.frame(chrom=c("chr19", "chr19"),
                                     start=c(44905872, 44905972),
                                     end  =c(44906072, 44906172),
                                     names=c("block1", "block2"),
                                     stringsAsFactors=FALSE)
               private$current.igv.instance$displayBedTrack("bed", tbl.bed, color="random", trackHeight=30)
               })

            observeEvent(input$loadBedGraphTrackButton, {
               printf("--- load bedGraph track")
               private$current.igv.instance$setLocus("APOE") # 44,904,796-44,910,393"
               scores <- 1:5000
               tbl.bg <- data.frame(chrom=rep("chr19", 5000),
                                     start=seq(44904172, 44904172+4999),
                                     end  =seq(44906173, 44906173+4999),
                                     score=scores,
                                     stringsAsFactors=FALSE)
               private$current.igv.instance$displayBedGraphTrack("bedGraph", tbl.bg,
                                                                 color="random",
                                                                 autoscale=TRUE,
                                                                 min=-2, max=10,
                                                                 trackHeight=30)
               })
            observeEvent(input$loadVcfTrackButton, {
               file <- system.file(package="igvWidget", "extdata", "inpp5d.fragment.vcf")
               printf("exists? %s: %s", file.exists(file), file)
               private$current.igv.instance$setLocus("chr2:232,984,561-233,283,684")
               vcfData <- readVcf(file)
               private$current.igv.instance$displayVcfTrack("vcf", vcfData)
               })
            observeEvent(input$loadSegTrackButton, {
               file <- system.file(package="igvWidget", "extdata", "GBM-TP.seg")

               printf("exists? %s: %s", file.exists(file), file)
               tbl.seg <- read.table(file, sep="\t", as.is=TRUE, header=TRUE)[-1,]
               private$current.igv.instance$displaySegTrack("SEG", tbl.seg)
               })
            observeEvent(input$loadGwasTrackButton, {
               file <- system.file(package="igvWidget", "extdata", "gwas.RData")
               printf("exists? %s: %s", file.exists(file), file)
               tbl.gwas <- get(load(file))
               printf("tbl.gwas, nrow: %d", nrow(tbl.gwas))
               private$current.igv.instance$setLocus("chr19:45,316,064-45,472,399")
               private$current.igv.instance$displayGwasTrack("GWAS", tbl.gwas)
               })

            observeEvent(input$popupButton, {
                showModal(modalDialog(renderTable(mtcars[1:5, 1:5], title="fubar")))
                })


            } # server
        #------------------------------------------------------------
       ) # public
    ) # class
#--------------------------------------------------------------------------------

app <- igvDemoApp$new()
x <- shinyApp(app$ui, app$server)
runApp(x, port=1111)

