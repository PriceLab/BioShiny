library(R6)
library(shiny)
library(dataTableWidget)
library(msgBoxWidget)
library(shinyjs)
#----------------------------------------------------------------------------------------------------
buttonStyle <- "margin: 5px; margin-right: 0px; font-size: 14px;"

DataTableDemoApp = R6Class("app",

    #--------------------------------------------------------------------------------
    private = list(dtw = NULL,
                   randomTextButton = NULL,
                   tbl = NULL,
                   msgBox = NULL),

    #--------------------------------------------------------------------------------
    public = list(

        initialize = function(){
            printf("initializing demo")
            private$msgBox =  msgBoxWidget$new(id="box1", title="table selection", boxWidth=600)
            private$tbl = mtcars [, 1:3] # [, c("mpg", "cyl", "disp")]
            private$dtw = dataTableWidget$new(id="tbl.1", private$tbl,
                                              width="100%", height="500px",
                                              border="1px blue solid; border-radius: 10px;")
                                              columnWidths=c(50,30,300)
            },

        #------------------------------------------------------------
        ui = function(){
            fluidPage(
               tags$header(shinyjs::useShinyjs()),
               titlePanel("Solve the Misaligned Column Titles Problem"),
               sidebarLayout(
                 sidebarPanel(
                    actionButton("refreshButton", "Refresh"),
                    sliderInput("maxMpgSlider", "Max MPG:",  min=10, max=35, value=35),
                    sliderInput("maxDispSlider", "Max Displacement:",  min=70, max=500, value=500)
                    ),
                 mainPanel(
                   wellPanel(style="width: 1000px;", private$msgBox$ui()),
                   div(private$dtw$ui(),
                       style="width: 500px; height: auto")
                   )
                 ) # sidebarLayout
              ) # fluidPage

              },

        #------------------------------------------------------------
        server = function(input, output, session){

            printf("entering dataTableWidgetDemo::server")
            private$msgBox$server(input, output, session)
            private$dtw$server(input, output, session)

            observeEvent(input$refreshButton, ignoreInit=TRUE,{
                         shinyjs::runjs("$('#tbl.1 table.dataTable').DataTable().draw();")
                         })
            observe({
               mpg.max <- input$maxMpgSlider
               disp.max <- input$maxDispSlider
               tbl.sub <- subset(private$tbl, mpg <= mpg.max & disp <= disp.max)
               printf("tbl.sub, %d rows, %d cals", nrow(tbl.sub), ncol(tbl.sub))
               private$dtw$setTable(tbl.sub)
               })

            observe({
               row.names <- private$dtw$tableSelection()
               #row.names <- rownames(private$tbl)[row.numbers]
               #print(row.names)
               private$msgBox$setText(paste(row.names, collapse=", "))
               })
            } # server

       ) # public
    ) # class
#--------------------------------------------------------------------------------
app <- DataTableDemoApp$new()
runApp(shinyApp(app$ui, app$server), port=1112)

