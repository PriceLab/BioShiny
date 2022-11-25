#' R6 Class representing a simple text display widget
#'
#' @title ProteomicsFilteringWidget
#' @description an R6 class providing clean access to a simple html message box
#' @name ProteomicsFilteringWidget
#'
#' @import shiny
#' @import R6
#' @import r2d3

printf <- function(...) print(noquote(sprintf(...)))

library(R6)

#' @export
ProteomicsFilteringWidget = R6Class("ProteomicsFilteringWidget",

    private = list(
        id = NULL,
        title = NULL,
        session = NULL,
        input = NULL,
        output = NULL,
        proteinCount = NULL,
        tbl.all = data.frame(),
        tbl.current = data.frame(),
        tbl.selected = data.frame(),
        tbl.complexes = data.frame(),
        complexes = NULL,
        fraction.names = NULL,
        currentProteins=NULL,
        transform="None",
        ns= NULL
        ),

        #' @description
        #' Create a new ProteomicsFilteringWidget
        #' @param id the html document div id
        #' @param title character
        #' @return A new `ProteomicsFilteringWidget` object.

   public = list(
       initialize = function(id, title="foo"){
          private$ns <- NS(id)
          printf("entering ProteomicsFilteringWidget::initialize, wednesday 11/23 921a")
          private$id <- id
          private$title <- title;
          f <- "~/github/erythro-dia/viz/v0/tbl.all-11492x14.RData"
          private$tbl.all <- tail(get(load(f)), n=-1)
          private$tbl.current <- private$tbl.all
          max.time.points <- 9
          fraction.names <- sort(unique(tbl.all$fraction))
          f <- "~/github/erythro-dia/viz/v0/tbl.complexes.RData"
          tbl.complexes <- get(load(f))
          private$tbl.complexes <- tbl.complexes
          private$complexes <- sort(unique(tbl.complexes$complex))
          private$fraction.names <- sort(unique(private$tbl.all$fraction))
          },

        #------------------------------------------------------------
        #' @description
        #' draw a protein vector
        #' @param input the shiny object, access to the ui
        #' @param output the shiny object, for drawing
        #' @return nothing

        plotProteins=function(input, output){
            #printf("plotProteins, nrow(tbl.selected): %d", nrow(private$tbl.selected))
            #printf("plotProteins, currentProteins:  %d", length(private$currentProteins))
            #print("--- tbl.selected")
            #print(head(private$tbl.selected))
            tbl.sub <- subset(private$tbl.selected, gene %in% private$currentProteins)
            #printf("plotProteins, nrow(tbl.sub): %d", nrow(tbl.sub))
            protein.fraction.names <- sprintf("%s-%s", tbl.sub$gene, tbl.sub$fraction) # eg, "A2M-cyto" & "A2M-ne1"
            mtx <- tbl.sub[, grep("^D", colnames(private$tbl.current), ignore.case=TRUE)] # just the time columns

            if(nrow(mtx) > 0 & private$transform == "Normalized"){
                mtx <- t(apply(mtx, 1, function(row) row/max(row)))
                }

            rownames(mtx) <- protein.fraction.names
            timePoints <- as.numeric(sub("D", "", colnames(mtx)))
            protein.count.vectors <- lapply(seq_len(nrow(mtx)), function(row) as.numeric(mtx[row,]))
            names(protein.count.vectors) <- protein.fraction.names

            xMin <- min(timePoints)
            xMax <- max(timePoints)
            yMin <- 0
            yMax <- max(mtx)

            vectorsWithTimes <- vector(mode="list", length(rownames(mtx)))
            names(vectorsWithTimes) <- rownames(mtx)

            for(protein.fraction.name in protein.fraction.names){
                vector <- protein.count.vectors[[protein.fraction.name]]
                vectorsWithTimes[[protein.fraction.name]] <-
                    lapply(seq_len(length(timePoints)),
                           function(i)
                               return(list(x=timePoints[i], y=vector[i])))
                } # for protein.fraction.name

            lineSmoothing <- private$input$srm.lineTypeSelector
            data <- list(vectors=vectorsWithTimes, xMax=xMax, yMax=yMax, cmd="plot",
                         smoothing=lineSmoothing, divID=private$ns("r2d3.pfw"))
            multiplot.script.path <- system.file(package="ProteomicsFilterWidget", "js", "multiplot.js")
            stopifnot(file.exists(multiplot.script.path))
            #         d3Output(private$d3.id, height="80vh"),
            # private$output$srm.d3 <- renderD3({
            #private$output[private$d3.id] <- renderD3({
            #browser()
            private$output$r2d3.pfw <- renderD3({
               r2d3(data, script = multiplot.script.path)
               })
            }, # plotProteins

        #------------------------------------------------------------
        #' @description
        #' find the maximum of several vectors
        #' @param vectorList list of numeric vectors
        #' @return numeric

        maxOfVectors = function(vectorList){
            max <- 0
            for(vector in vectorList){
                vector.max <- max(vector, na.rm=TRUE)
                                        #if(is.na(vector.max)) browser()
                if(vector.max > max)
                    max <- vector.max
                } # for vector
            return(max)
            }, # maxOfVectors


        #------------------------------------------------------------
        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
        ui = function(){
          fluidPage(
             br(), br(),
             sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput(inputId=private$ns("fractionSelector"),
                                        label="Choose Cellular Fractions:",
                                        choices = private$fraction.names,
                                        selected = private$fraction.names[1:3]),
                     selectizeInput(inputId=private$ns("complexSelector"),
                                    "Choose Complexes:", private$complexes, selected=NULL,
                                    multiple=TRUE,
                                    options=list(maxOptions=length(private$complexes))),
                     h6("Filtered Set Size:"),
                     verbatimTextOutput(outputId=private$ns("currentCurveCountDisplay")),
                     tags$head(tags$style(HTML("#currentCurveCountDisplay {font-size: 12px;}"))),
                     hr(),
                     h6("Choose Proteins From Filtered Set:"),
                     selectizeInput(inputId=private$ns("proteinSelector"),
                                    label=NULL,
                                    choices=NULL, #sort(unique(private$tbl.current$gene)),
                                    selected=NULL,
                                    multiple=TRUE,
                                    options=list(maxOptions=nrow(private$tbl.current))),
                     verbatimTextOutput(outputId=private$ns("currentSubsetCountDisplay")),
                     actionButton(inputId=private$ns("plotCurrentSelectionButton"), "Plot Current Selection"),
                     br(),
                     radioButtons(inputId=private$ns("srm.transformChoice"), "Data Transform", c("None", "Normalized")), # , "Arcsinh")),
                     verbatimTextOutput(outputId=private$ns("currentVectorDisplay")),
                     width=3
                 ),
                   #  eval(parse(text=sprintf("%s <- '%s'", tolower(as.character(tmp)), names(tmp))))
                 mainPanel(
                     d3Output(outputId=private$ns("r2d3.pfw"), height="80vh", width="100%"),
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
         moduleServer(private$id, function(input, output, session){
         print(noquote(sprintf("entering ProteomicsFilteringWidget server")))
         print(noquote(sprintf("entering ProteomicsFilteringWidget server %s", "r2d3.pfw")))
         private$session = session;
         private$input = input
         private$output = output


         observeEvent(private$input$proteinSelector, ignoreInit=FALSE, {
             proteins <- private$input$proteinSelector
             private$currentProteins <- proteins
                private$tbl.selected <- subset(private$tbl.current, gene %in% proteins)
             row.count <- nrow(private$tbl.selected)
             printf("tbl.selected has %d rows", row.count)
             text <- sprintf("%d rows, %d proteins", row.count, length(proteins))
             private$output$currentSubsetCountDisplay <- renderText(text)
             })


         currentTable <- reactive({
             printf("------------------------------------------- entering currentTable()")
             tbl.tmp <- private$tbl.all
             complexes <- private$input$complexSelector
             printf("--- complexes: ");
             print (complexes)
             fractions <- private$input$fractionSelector
             printf("--- fractions: ");
             print (fractions)
             current.proteins <- isolate(private$input$proteinSelector)
             printf("current proteins (if any): %s", paste(current.proteins, collapse=","))

             printf("==== nrow(tbl.tmp) 1: %d", nrow(tbl.tmp))

             printf("complexes: %s", paste(complexes, collapse=", "))
             if(!is.null(complexes)){
                 printf("filtering on chosen complexes")
                 tbl.complexes.sub <- subset(private$tbl.complexes, complex %in% complexes)
                 printf(" nrow(tbl.complexes.sub): %d", nrow(tbl.complexes.sub))
                 tbl.tmp <- subset(tbl.tmp, gene %in% tbl.complexes.sub$gene)
                 printf("tbl.tmp, filtered for complexes, now has %d rows", nrow(tbl.tmp))
                 }
             printf("==== nrow(tbl.tmp) 2: %d", nrow(tbl.tmp))
             printf("fractions: %s", paste(fractions, collapse=", "))
             if(length(fractions) == 0)
                 tbl.tmp <- subset(tbl.tmp, fraction == "none specified")
             printf("==== nrow(tbl.tmp) 3: %d", nrow(tbl.tmp))
             if(length(fractions) > 0)
                 tbl.tmp <- subset(tbl.tmp, fraction %in% fractions)

             printf("==== nrow(tbl.tmp) 4: %d", nrow(tbl.tmp))

             printf("==== nrow(tbl.tmp) 5: %d", nrow(tbl.tmp))
             printf("==== nrow(tbl.tmp) 6: %d", nrow(tbl.tmp))
             private$tbl.current <- tbl.tmp
             return(nrow(private$tbl.current))
         }) # currentTable

         observe({
             row.count <- currentTable()
             current.proteins <- isolate(private$input$proteinSelector)
             unique.proteins <- sort(unique(private$tbl.current$gene))
             current.proteins.still.available <- intersect(current.proteins, unique.proteins)
             printf("    unique.proteins: %d", length(unique.proteins))
             updateSelectizeInput(session = private$session,
                                  inputId = "proteinSelector",
                                  choices = unique.proteins,
                                  selected = current.proteins.still.available,
                                  server=TRUE
                                  )
             printf("--- observe, new row count: %d", row.count)
             private$currentProteins <- unique.proteins
             private$tbl.selected <- subset(private$tbl.current, gene %in% unique.proteins)
             private$output$currentCurveCountDisplay <-
                 renderText(sprintf("%d rows, %d proteins", row.count, length(unique.proteins)))
             })

         observeEvent(private$input$plotCurrentSelectionButton, ignoreInit=TRUE, {
             printf("--- plotCurrentSelectionButton")
             printf("plot %d proteins", length(private$currentProteins))
             self$plotProteins(private$input, private$output)
             })

         observeEvent(private$input$srm.transformChoice, ignoreInit=TRUE, {
             new.choice <- private$input$srm.transformChoice
             #printf("--- setting private$transform: %s", new.choice)
             private$transform <- new.choice
             self$plotProteins(private$input, private$output)
             })


         observeEvent(private$input$currentlySelectedVector, ignoreInit=FALSE, {
             #printf("--- observing new currentlySelectedVector")
             newValue <- private$input$currentlySelectedVector
             if(nchar(newValue) > 0){
                 tokens <- strsplit(newValue, "-")[[1]]
                 current.gene <- tokens[1]
                 current.fraction <- tokens[2]
                 peptideCount <- subset(private$tbl.current,
                                        gene==current.gene &
                                        fraction==current.fraction)$peptideCount
                 newValue <- sprintf("%s-%dp", newValue, peptideCount)
                                        #printf(" peptideCount added: %s", newValue)
                 private$output$currentVectorDisplay <- renderText({newValue})
                } # if a new character string received.  don't clear: leave it displayed
            })
         }) # moduleServer
      } # server


     ) # public
  ) # class

