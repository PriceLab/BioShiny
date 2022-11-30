#' R6 Class representing a simple text display widget
#'
#' @title ProteomicsFilteringWidget
#' @description an R6 class providing clean access to a simple html message box
#' @name ProteomicsFilteringWidget
#'
#' @import shiny
#' @import R6
#' @import r2d3
#' @import shinyjs

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
        currentProteins = NULL,
        lastSelectedProtein = NULL,
        transform = "None",
        correlationExcludableTimepoints = c(),
        correlationExcludedTimepoints = c(),
        correlationThreshold = 1,
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
          private$tbl.all <- get(load(f))
          private$tbl.current <- private$tbl.all
          max.time.points <- 9
          private$correlationExcludableTimepoints <- grep("^D", colnames(private$tbl.all), value=TRUE)
          private$correlationExcludedTimepoints <- c()
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
        #' @param mtx extracted from tbl.all, just the Dn timepoint columns, just the selected
        #         protein/fraction rows
        #' @return nothing

        plotProteins=function(input, output, mtx){
            timePoints <- as.numeric(sub("D", "", colnames(mtx)))
            protein.count.vectors <- lapply(seq_len(nrow(mtx)), function(row) as.numeric(mtx[row,]))
            names(protein.count.vectors) <- rownames(mtx)

            xMin <- min(timePoints)
            xMax <- max(timePoints)
            yMin <- 0
            yMax <- max(mtx)

            vectorsWithTimes <- vector(mode="list", length(rownames(mtx)))
            names(vectorsWithTimes) <- rownames(mtx)

            for(protein.fraction.name in rownames(mtx)){
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
             shinyjs::useShinyjs(),
             br(), br(),
             sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput(inputId=private$ns("fractionSelector"),
                                        label="Choose Cellular Fractions:",
                                        choices = private$fraction.names,
                                        selected = private$fraction.names[1:3],
                                        inline=TRUE),
                     selectizeInput(inputId=private$ns("complexSelector"),
                                    "Choose Complexes:", private$complexes, selected=NULL,
                                    multiple=TRUE,
                                    options=list(maxOptions=length(private$complexes))),
                     h6("Filtered Set Size:"),
                     verbatimTextOutput(outputId=private$ns("currentCurveCountDisplay")),
                     tags$head(tags$style(HTML("#currentCurveCountDisplay {font-size: 12px;}"))),
                     h6("Choose Proteins From Filtered Set:"),
                     selectizeInput(inputId=private$ns("proteinSelector"),
                                    label=NULL,
                                    choices=NULL, #sort(unique(private$tbl.current$gene)),
                                    selected=NULL,
                                    multiple=TRUE,
                                    options=list(maxOptions=nrow(private$tbl.current))),
                     verbatimTextOutput(outputId=private$ns("currentSubsetCountDisplay")),
                     actionButton(inputId=private$ns("plotCurrentSelectionButton"), "Plot Current Selection"),
                     br(), br(),
                     radioButtons(inputId=private$ns("srm.transformChoice"),
                                  "Desired Data Transform", c("None", "Normalized"), inline=TRUE), # , "Arcsinh")),
                     div(style=paste("background: lightyellow",
                                     "height: 40px",
                                     "width: 300px",
                                     "border: 1px solid black",
                                     "margin-bottom: 24px",
                                     "font-size: 22px",
                                     "padding: 3px",
                                     "padding-left: 10px",
                                     sep=";"),
                         textOutput(outputId=private$ns("currentVectorDisplay"))
                         ),
                     wellPanel(style = "background: #E3DFD8",
                       radioButtons(inputId=private$ns("correlationDirectionChooser"),
                                    label="Find Spearman Correlations",
                                    c("+", "-"), inline=TRUE),
                       selectizeInput(inputId=private$ns("timepointExclusionChooser"),
                                      label="Exclude these Timepoints:",
                                      choices=private$correlationExcludableTimepoints,
                                      selected=NULL,
                                      multiple=TRUE,
                                      options=list(maxOptions=length(private$correlationExcludableTimepoints))),
                       sliderInput(inputId=private$ns("correlationThresholdSlider"),
                                   label="",
                                   min=0, max=1, value=0.99, step=0.01),
                       actionButton(inputId=private$ns("plotCorrelatedButton"), "Plot Correlated")
                       ),
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
            shinyjs::disable("plotCorrelatedButton")


            observeEvent(private$input$correlationDirectionChooser, ignoreInit=TRUE, {
                newDirection <- private$input$correlationDirectionChooser
                printf("correlation direction: %s", newDirection)
                if(!is.null(private$lastSelectedProtein)){
                   printf("  correlation target: %s", private$lastSelectedProtein)
                   }
                })

            observeEvent(private$input$proteinSelector, ignoreInit=FALSE, {
                proteins <- private$input$proteinSelector
                private$currentProteins <- proteins
                private$tbl.selected <- subset(private$tbl.current, gene %in% proteins)
                row.count <- nrow(private$tbl.selected)
                printf("tbl.selected has %d rows", row.count)
                text <- sprintf("%d rows, %d protein/s", row.count, length(proteins))
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
               tbl.sub <- subset(private$tbl.selected, gene %in% private$currentProteins)
               protein.fraction.names <- sprintf("%s-%s", tbl.sub$gene, tbl.sub$fraction) # eg, "A2M-cyto" & "A2M-ne1"
               mtx <- tbl.sub[, grep("^D", colnames(private$tbl.current), ignore.case=TRUE)] # just the time columns
               if(nrow(mtx) > 0 & private$transform == "Normalized"){
                   mtx <- t(apply(mtx, 1, function(row) row/max(row)))
                   }
               rownames(mtx) <- protein.fraction.names
               self$plotProteins(private$input, private$output, mtx)
               })

            observeEvent(private$input$srm.transformChoice, ignoreInit=TRUE, {
               new.choice <- private$input$srm.transformChoice
                                       #printf("--- setting private$transform: %s", new.choice)
               private$transform <- new.choice
               #self$plotProteins(private$input, private$output)
               })


            observeEvent(private$input$currentlySelectedVector, ignoreInit=TRUE, {
                newValue <- private$input$currentlySelectedVector
                printf("--- currentlySelectedVector update: '%s'", newValue)
                if(nchar(newValue) == 0){
                    shinyjs::disable("plotCorrelatedButton")
               } else {
                   shinyjs::enable("plotCorrelatedButton")
                   tokens <- strsplit(newValue, "-")[[1]]
                   current.gene <- tokens[1]
                   current.fraction <- tokens[2]
                   printf("  current.gene: '%s'", current.gene)
                   printf("      fraction: '%s'", current.fraction)
                   peptideCount <- subset(private$tbl.all,
                                          gene==current.gene &
                                          fraction==current.fraction)$peptideCount
                   printf("   peptideCount: ")
                   print(peptideCount)
                   newValue <- sprintf("%s-%dp", newValue, peptideCount)
                   private$lastSelectedProtein <- newValue
                   private$output$currentVectorDisplay <- renderText({newValue})
                   } # if a new character string received.  don't clear: leave it displayed
                })

            observeEvent(private$input$timepointExclusionChooser, ignoreInit=TRUE, {
               timepoints <- private$input$timepointExclusionChooser
               printf("entering timepoint exclusion: %s", paste(timepoints, collapse=","))
               private$correlationExcludedTimepoints <- timepoints
               printf("exlude %s", paste(timepoints, collapse=","))
               })

            observeEvent(private$input$plotCorrelatedButton, ignoreInit=TRUE, {
               printf("--- plot correlated");
               tokens <- strsplit(private$lastSelectedProtein, "-")[[1]]
               target.gene <- tokens[1]
               target.fraction <- tokens[2]
               threshold <- isolate(private$input$correlationThresholdSlider)
               direction <- isolate(private$input$correlationDirectionChooser)
               excluded.timepoints <- private$correlationExcludedTimepoints
               printf("number of exclude timepoints: %d", length(excluded.timepoints))
                 # when looking for correlated proteins, consider all fractions
               # browser()
               tbl.sub <- private$tbl.all
               tbl.target <- subset(tbl.sub, gene==target.gene & fraction==target.fraction)
               printf("should be only 1 target row, found %d", nrow(tbl.target))
               numeric.columns <- grep("^D", colnames(private$tbl.current), ignore.case=TRUE)
               excluded.columns <- c()
               if(length(excluded.timepoints) > 0){
                   excluded.columns <- match(excluded.timepoints, colnames(private$tbl.current))
                   }
               numeric.columns <- setdiff(numeric.columns, excluded.columns)
               target.vector <- as.numeric(tbl.target[1, numeric.columns])
               mtx.row.names <- sprintf("%s-%s", tbl.sub$gene, tbl.sub$fraction)
               dups <- which(duplicated(mtx.row.names))  # todo: make sure this never happens
               if(length(dups) > 0){
                   tbl.sub <- tbl.sub[-dups,]
                   mtx.row.names <- mtx.row.names[-dups]
                   }
               mtx <- tbl.sub[, numeric.columns]
               rownames(mtx) <- mtx.row.names
               suppressWarnings(
                   correlations <- apply(mtx, 1,
                                         function(row) cor(target.vector, row,  use="complete.obs")))
               if(direction == "-")
                  result <- names(which(correlations <= (-1 * threshold)))
               else  # must be "+"
                  result <- names(which(correlations >= threshold))
                  # make sure the target protein-fraction is in the list, needed for negative correlations
               result <- unique(c(result, sprintf("%s-%s", target.gene, target.fraction)))
               printf("--- rows in mtx.correlated: %d", length(result))
               if(length(result) > 0){
                  mtx.correlated <- mtx[result,]
                  if(private$transform == "Normalized"){
                     mtx.correlated <- t(apply(mtx.correlated, 1, function(row) row/max(row)))
                     }
                  self$plotProteins(private$input, private$output, mtx.correlated)
                  } # if some found

               }) # observeEvent: plotCorrelatedButton

         }) # moduleServer
      } # server

     ) # public
  ) # class

