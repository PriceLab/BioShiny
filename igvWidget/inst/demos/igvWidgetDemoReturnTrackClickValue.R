library(R6)
library(shiny)
library(shinyBS)
library(VariantAnnotation)
library(igvWidget)
library(shinyModules)  # for messageBox, until it becomes a biocShiny widget
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
        #------------------------------------------------------------
        initialize = function(){
            self$igv = igvWidget$new("igv01",
                                         genome="hg38",
                                         locus="APOE",
                                         width=1000,
                                         height=400,
                                         border="1px solid purple; border-radius: 5px;")
            private$current.igv.instance <- self$igv
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
                  column(1, actionButton("loadGwasTrackButton", label="GWAS", style=buttonStyle))
                  )),
               messageBoxUI(id="messageBox.1", title="Track Click Entity"),
               self$igv$ui()
            )},

        #------------------------------------------------------------
        server = function(input, output, session){

            self$igv$server(input, output, session)

            messageBox1.contents <- messageBoxServer("messageBox.1",
                                                     newContent=self$igv$trackClickResult)

            observeEvent(input$newLocusButton, {
               index = sample(seq_len(length(private$loci.miscellany)), 1)
               newLocus = private$loci.miscellany[index]
               private$current.igv.instance$setLocus(newLocus)
               })

            observeEvent(input$trackClick, {
               x <- input$trackClick
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
                                       # use grep so that "name" and "Name" and "NAME" are all equally discoverable
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
               printf("returning selected entity")
               print(entity)
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
               file <- system.file(package="shinyModules", "extdata", "inpp5d.fragment.vcf")
               printf("exists? %s: %s", file.exists(file), file)
               private$current.igv.instance$setLocus("chr2:232,984,561-233,283,684")
               vcfData <- readVcf(file)
               private$current.igv.instance$displayVcfTrack("vcf", vcfData)
               })
            #observeEvent(input$loadSegTrackButton, {
            #   file <- system.file(package="shinyModules", "extdata", "GBM-TP.seg")
            #   printf("exists? %s: %s", file.exists(file), file)
            #   tbl.seg <- read.table(file, sep="\t", as.is=TRUE, header=TRUE)[-1,]
            #   private$current.igv.instance$displaySegTrack("SEG", tbl.seg)
            #   })
            observeEvent(input$loadGwasTrackButton, {
               file <- system.file(package="shinyModules", "extdata", "gwas.RData")
               printf("exists? %s: %s", file.exists(file), file)
               tbl.gwas <- get(load(file))
               printf("tbl.gwas, nrow: %d", nrow(tbl.gwas))
               private$current.igv.instance$setLocus("chr19:45,316,064-45,472,399")
               private$current.igv.instance$displayGwasTrack("GWAS", tbl.gwas)
               })

            } # server
        #------------------------------------------------------------
       ) # public
    ) # class
#--------------------------------------------------------------------------------

app <- igvDemoApp$new()
x <- shinyApp(app$ui, app$server)
runApp(x, port=1111)

