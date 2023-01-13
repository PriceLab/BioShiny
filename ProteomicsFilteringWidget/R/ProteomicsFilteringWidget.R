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

# from https://betterprogramming.pub/heres-why-i-m-replacing-html2canvas-with-html-to-image-in-our-react-app-d8da0b85eadf
jsCode <- "saveAs = function (blob, fileName){
               var elem = window.document.createElement('a');
               elem.href = blob
               elem.download = fileName;
               elem.style = 'display:none;';
               (document.body || document.documentElement).appendChild(elem);
               if (typeof elem.click === 'function') {
                   elem.click();
               } else {
                   elem.target = '_blank';
                   elem.dispatchEvent(new MouseEvent('click', {
                      view: window, bubbles: true, cancelable: true}));
                   } // else
                 URL.revokeObjectURL(elem.href);
                 elem.remove()
                 }; // saveAs
            shinyjs.saveCanvasToJPG = function(filename){
               console.log('save ' + divID + ' to ' + filename);
               htmlToImage.toJpeg(document.getElementById(divID))
                 .then(function (dataUrl) {
                     saveAs(dataUrl, filename);
                     });
               }; //saveCanvasToJPG"


#' @export
ProteomicsFilteringWidget = R6Class("ProteomicsFilteringWidget",

    private = list(
        id = NULL,
        title = NULL,
        session = NULL,
        input = NULL,
        output = NULL,
        tbl.all = data.frame(),
        tbl.current = data.frame(),
        tbl.selected = data.frame(),
        tbl.complexes = data.frame(),
        complexes = NULL,
        fraction.names = NULL,
        currentFractions = c(),
        currentComplexes = c(),
        currentProteins = NULL,
        lastSelectedProtein = NULL,
        transform = "Normalized",
        correlationMethod = "spearman",
        correlationDirection = "+",
        correlationExcludableTimepoints = c(),
        correlationExcludedTimepoints = c(),
        correlationThreshold = 1,
        ns= NULL
        ),

        #' @description
        #' Create a new ProteomicsFilteringWidget
        #' @param id the html document div id
        #' @param tbl.data data.frame, idiosyncratic format for DIA data
        #' @param tbl.complexes data.frame, associates proteins (by gene name) from tbl.data into complexes
        #' @param title character
        #' @return A new `ProteomicsFilteringWidget` object.

   public = list(
       initialize = function(id, tbl.data, tbl.complexes, title="foo"){
          private$ns <- NS(id)
          private$id <- id
          private$title <- title;
          private$tbl.all <- tbl.data
          private$tbl.current <- private$tbl.all
          max.time.points <- 9
          private$correlationExcludableTimepoints <- grep("^D", colnames(private$tbl.all), value=TRUE)
          private$correlationExcludedTimepoints <- c()
          fraction.names <- sort(unique(private$tbl.all$fraction))
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
             shiny::tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/html-to-image/1.7.0/html-to-image.js"),
             shinyjs::useShinyjs(),
             extendShinyjs(text=jsCode, functions=c("saveAs", "saveCanvasToJPG")),
             br(), br(),
             sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput(inputId=private$ns("fractionSelector"),
                                        label="Choose Cellular Fractions:",
                                        choices = private$fraction.names,
                                        selected = private$fraction.names[1:3],
                                        inline=TRUE),
                     selectizeInput(inputId=private$ns("complexSelector"),
                                    label="Choose Complexes:",
                                    choices=private$complexes,
                                    selected=NULL,
                                    multiple=TRUE,
                                    options=list(maxOptions=length(private$complexes),
                                                 placeholder="Use all proteins, ignoring complexes")),
                     h6("Filtered Set Size:"),
                     verbatimTextOutput(outputId=private$ns("currentCurveCountDisplay")),
                     tags$head(tags$style(HTML("#currentCurveCountDisplay {font-size: 12px;}"))),
                     h6("Choose Proteins From Filtered Set:"),
                     selectizeInput(inputId=private$ns("proteinSelector"),
                                    label=NULL,
                                    choices=NULL, #sort(unique(private$tbl.current$gene)),
                                    selected=NULL,
                                    multiple=TRUE,
                                    options=list(maxOptions=nrow(private$tbl.current),
                                                 placeholder="Use all in current filtered set")),
                     #verbatimTextOutput(outputId=private$ns("currentSubsetCountDisplay")),
                     actionButton(inputId=private$ns("plotCurrentSelectionButton"), "Plot Current Selection"),
                     actionButton(inputId=private$ns("savePlotButton"), label="Save Plot"),
                     br(), br(),
                     radioButtons(inputId=private$ns("transformChoice"),
                                  "Desired Data Transform",
                                  c("None", "Normalized"),
                                  selected="Normalized",
                                  inline=TRUE), # , "Arcsinh")),
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
                       radioButtons(inputId=private$ns("correlationMethodChooser"),
                                    label="Find Correlations",
                                    choices=c("Spearman", "Pearson"),
                                    inline=TRUE),
                       radioButtons(inputId=private$ns("correlationDirectionChooser"),
                                    label=NULL,
                                    choices=c("+", "-"),
                                    inline=TRUE),
                       selectizeInput(inputId=private$ns("timepointExclusionChooser"),
                                      label="Exclude these Timepoints:",
                                      choices=c(private$correlationExcludableTimepoints),
                                      selected=NULL,
                                      multiple=TRUE,
                                      options=list(maxOptions=length(private$correlationExcludableTimepoints),
                                                   allowEmptyOption=TRUE,
                                                   placeholder="Use all timepoints")),
                       sliderInput(inputId=private$ns("correlationThresholdSlider"),
                                   label="",
                                   min=0, max=1, value=0.99, step=0.01),
                       actionButton(inputId=private$ns("plotCorrelatedButton"), "Plot Correlated")
                       ),
                     width=3
                     ),
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
            private$session = session;
            private$input = input
            private$output = output
            shinyjs::disable("plotCorrelatedButton")


            #--------------------------------------------------------------------
            # spearman or pearson?
            #--------------------------------------------------------------------
            observeEvent(private$input$correlationMethodChooser, ignoreInit=FALSE, {
                newMethod <- private$input$correlationMethodChooser
                private$correlationMethod <- tolower(newMethod)
                })
            #--------------------------------------------------------------------
            # + or -?
            #--------------------------------------------------------------------
            observeEvent(private$input$correlationDirectionChooser, ignoreInit=TRUE, {
                newDirection <- private$input$correlationDirectionChooser
                private$correlationDirection <- newDirection
                })


            #--------------------------------------------------------------------
            # leave these timepoints out of the correlation calculation
            #--------------------------------------------------------------------
            observeEvent(private$input$timepointExclusionChooser, ignoreInit=TRUE, ignoreNULL=FALSE, {
                newTimepoints <- private$input$timepointExclusionChooser
                printf("--- new timepointExclusionChooser action")
                print(newTimepoints)
                if(all(is.null(newTimepoints))){
                    printf("    correcting null newTimepoints")
                    newTimepoints <- c()
                    }
                private$correlationExcludedTimepoints <- newTimepoints
                })

            #--------------------------------------------------------------------
            # one or many, or NULL, from the selectize dropdown.
            #--------------------------------------------------------------------
            observeEvent(private$input$proteinSelector, ignoreInit=FALSE, ignoreNULL=FALSE, {
                proteins <- private$input$proteinSelector
                if(is.null(proteins))
                    proteins <-
                printf("--- proteinSelector activated")
                print(proteins)
                private$currentProteins <- proteins
                private$tbl.selected <- subset(private$tbl.current, gene %in% proteins)
                row.count <- nrow(private$tbl.selected)
                })


            #--------------------------------------------------------------------
            #
            #--------------------------------------------------------------------
            observeEvent(private$input$fractionSelector, ignoreInit=TRUE, ignoreNULL=FALSE, {
                newValue <- private$input$fractionSelector
                printf("--- new fraction selected")
                print(newValue)
                private$currentFractions <- newValue
                })

            #--------------------------------------------------------------------
            #
            #--------------------------------------------------------------------
            observeEvent(private$input$complexSelector, ignoreInit=TRUE, ignoreNULL=FALSE, {
                newValue <- private$input$complexSelector
                printf("--- new complex selected")
                print(newValue)
                private$currentComplexes <- newValue
                })

            #--------------------------------------------------------------------
            #
            #--------------------------------------------------------------------
            currentTable <- reactive({
                printf("------------------------------------------- entering currentTable()")
                tbl.tmp <- private$tbl.all
                private$currentComplexes <- private$input$complexSelector
                private$currentFractions <- private$input$fractionSelector
                current.proteins <- isolate(private$input$proteinSelector)
                   #-------------------------------------------------------------------
                   # every protein has a specified fraction: no NULL case to accomodate
                   #-------------------------------------------------------------------
                tbl.tmp <- subset(tbl.tmp, fraction %in% private$currentFractions)
                printf("tbl.tmp, filtered for fractions, now has %d rows", nrow(tbl.tmp))
                   #-------------------------------------------------------------------
                   # in contrast to fractions, complexes == NULL means use all protesin
                   #-------------------------------------------------------------------
                if(!all(is.null(private$currentComplexes))){
                    tbl.complexes.sub <- subset(private$tbl.complexes, complex %in% private$currentComplexes)
                    printf("filtering on chosen complexes")
                    tbl.tmp <- subset(tbl.tmp, gene %in% tbl.complexes.sub$gene)
                    printf("tbl.tmp, filtered for complexes, now has %d rows", nrow(tbl.tmp))
                    }
                printf("==== nrow(tbl.tmp) 4: %d", nrow(tbl.tmp))
                #text <- sprintf("%d rows, %d protein/s", nrow(tbl.tmp), length(unique(tbl.tmp$gene)))
                #private$output$currentSubsetCountDisplay <- renderText(text)
                private$tbl.current <- tbl.tmp
                return(nrow(private$tbl.current))
                }) # currentTable

            #--------------------------------------------------------------------
            #  triggered by reactive function currentTable()
            #--------------------------------------------------------------------
            observe({
                row.count <- currentTable()
                current.proteins <- isolate(private$input$proteinSelector)
                unique.proteins <- sort(unique(private$tbl.current$gene))
                if(all(is.null(current.proteins)))
                    current.proteins <- unique.proteins  # NULL means no selection has been made: use all
                current.proteins.still.available <- intersect(current.proteins, unique.proteins)
                printf("    unique.proteins: %d", length(unique.proteins))
                # unique.proteins <- head(unique.proteins, n=50)
                updateSelectizeInput(session = private$session,
                                     inputId = "proteinSelector",
                                     choices = unique.proteins,
                                     # selected = current.proteins.still.available,
                                     #server=FALSE
                                     server=TRUE
                                     )
                printf("--- observe, new row count: %d", row.count)
                private$currentProteins <- unique.proteins
                private$tbl.selected <- subset(private$tbl.current, gene %in% unique.proteins)
                private$output$currentCurveCountDisplay <-
                    renderText(sprintf("%d rows, %d proteins", row.count, length(unique.proteins)))
                })

            observeEvent(private$input$savePlotButton, ignoreInit=TRUE,{
                printf("--- savePlot");
                js$saveCanvasToJPG("plot.jpg")
                })

            #--------------------------------------------------------------------
            #  depends on previously assigned private$currentProteins
            #--------------------------------------------------------------------
            observeEvent(private$input$plotCurrentSelectionButton, ignoreInit=FALSE, {
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

            #----------------------------------------------------------------------------
            # track data transformation choice, currently just "None" or "Normalized"
            #----------------------------------------------------------------------------
            observeEvent(private$input$transformChoice, ignoreInit=TRUE, {
               new.choice <- private$input$transformChoice
               private$transform <- new.choice
               })


            #--------------------------------------------------------------------
            #
            #--------------------------------------------------------------------
            observeEvent(private$input$currentlySelectedVector, ignoreInit=TRUE, {
                newValue <- private$input$currentlySelectedVector
                if(nchar(newValue) == 0){
                    shinyjs::disable("plotCorrelatedButton")
                } else {
                   shinyjs::enable("plotCorrelatedButton")
                   tokens <- strsplit(newValue, "-")[[1]]
                   current.gene <- tokens[1]
                   current.fraction <- tokens[2]
                   peptideCount <- subset(private$tbl.all,
                                          gene==current.gene &
                                          fraction==current.fraction)$peptideCount
                   newValue <- sprintf("%s-%dp", newValue, peptideCount)
                   private$lastSelectedProtein <- newValue
                   private$output$currentVectorDisplay <- renderText({newValue})
                   } # if a new character string received.  don't clear: leave it displayed
                })

            #--------------------------------------------------------------------
            #
            #--------------------------------------------------------------------
            observeEvent(private$input$plotCorrelatedButton, ignoreInit=TRUE, {
               printf("--- plot correlated");
               printf("   tbl.current: %d rows", nrow(private$tbl.current))

                  #------------------------------------------------------------
                  # identify the vector to which we want to find correlates
                  # gene (protein) name & fraction should be unique
                  #------------------------------------------------------------

               tokens <- strsplit(private$lastSelectedProtein, "-")[[1]]
               target.gene <- tokens[1]
               target.fraction <- tokens[2]

                  #------------------------------------------------------------
                  # now extract threshold, cor +/- & excluded timpoints
                  #------------------------------------------------------------

               threshold <- isolate(private$input$correlationThresholdSlider)
               direction <- private$correlationDirection
               method <- private$correlationMethod
               printf("   %s %s", method, direction)
               excluded.timepoints <- private$correlationExcludedTimepoints
               tbl.current <- private$tbl.current
               mtx.correlated <- self$findCorrelations(target.gene, target.fraction,
                                                       threshold, direction, method,
                                                       excluded.timepoints, tbl.current)

               if(nrow(mtx.correlated) > 0)
                   self$plotProteins(private$input, private$output, mtx.correlated)
               }) # observeEvent: plotCorrelatedButton

              }) # moduleServer
           },  # server

       #--------------------------------------------------------------------
       #
       #--------------------------------------------------------------------

        #' @description finds proteins/genes matching correlation criteria
        #' @param target.gene character
        #' @param target.fraction character
        #' @param threshold numeric
        #' @param direction character
        #' @param method character
        #' @param excluded.timepoints vector
        #' @param tbl.current data.frame
        #' @returns matrix

       findCorrelations = function(target.gene, target.fraction, threshold, direction,
                                   method, excluded.timepoints, tbl.current){

           printf("--- entering findCorrelations")
           printf("method: %s", method)
           printf("direction: %s", direction)
           printf("threshold: %f", threshold)
           printf("---  excluded.timepoints straight from selectize")
           print(excluded.timepoints)
           printf("number of exclude timepoints: %d", length(excluded.timepoints))
           tbl.target <- subset(private$tbl.all, gene==target.gene & fraction==target.fraction)
           printf("should be only 1 target row, found %d", nrow(tbl.target))
           numeric.columns <- grep("^D", colnames(private$tbl.current), ignore.case=TRUE)
           excluded.columns <- c()
           if(length(excluded.timepoints) > 0){
              excluded.columns <- match(excluded.timepoints, colnames(private$tbl.current))
              }
           numeric.columns <- setdiff(numeric.columns, excluded.columns)
           target.vector <- as.numeric(tbl.target[1, numeric.columns])
           mtx.row.names <- sprintf("%s-%s", tbl.current$gene, tbl.current$fraction)
           dups <- which(duplicated(mtx.row.names))  # todo: make sure this never happens
           if(length(dups) > 0){
              tbl.current <- tbl.current[-dups,]
              mtx.row.names <- mtx.row.names[-dups]
              }
           mtx <- tbl.current[, numeric.columns]
           rownames(mtx) <- mtx.row.names
           base.vector.name <- sprintf("%s-%s", target.gene, target.fraction)

                    # we always want to display the original target vector. add it in if omitted
                    # due to looking for correlates in another fraction or complex
           if(!base.vector.name %in% rownames(mtx)){
               mtx.target <- matrix(data=target.vector, nrow=1,
                                    dimnames=list(base.vector.name, colnames(mtx)))
               mtx <- rbind(mtx, mtx.target)
               }
           suppressWarnings(
               correlations <- apply(mtx, 1,
                           function(row) cor(target.vector, row,  use="complete.obs", method=method)))
           if(direction == "-")
               result <- names(which(correlations <= (-1 * threshold)))
           else  # must be "+"
               result <- names(which(correlations >= threshold))
              #------------------------------------------------------------
              # make sure the target protein-fraction is in the list,
              # needed for negative correlations, and in case the target is
              # not contained in the current search set, tbl.current
              #------------------------------------------------------------
           result <- unique(c(result, base.vector.name))
           printf("--- rows in mtx.correlated: %d", length(result))
           printf("    %s", paste(result, collapse=", "))
           if(length(result) == 0) return(matrix())
           mtx.correlated <- mtx[result,]
           if(private$transform == "Normalized"){
               mtx.correlated <- t(apply(mtx.correlated, 1, function(row) row/max(row)))
               }
           invisible(mtx.correlated)
           } # findCorrelations
           #----------------------------------------------------------------------

     ) # public

  ) # class

