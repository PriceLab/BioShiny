library(R6)
library(shiny)
library(dataTableWidget)
library(msgBoxWidget)
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
            private$tbl = mtcars
            private$dtw = dataTableWidget$new(id="tbl.1", private$tbl,
                                                     width="90%", height="500px",
                                                     border="1px blue solid; border-radius: 10px;")
            },

        #------------------------------------------------------------
        ui = function(){
           fluidPage(
              wellPanel(style="width: 1000px;",
                private$msgBox$ui()),
                sliderInput("maxMpgSlider", "Max MPG:",  min=10, max=35, value=35),
                sliderInput("maxDispSlider", "Max MPG:",  min=70, max=500, value=500),
                private$dtw$ui()
              )},

        #------------------------------------------------------------
        server = function(input, output, session){

            printf("entering dataTableWidgetDemo::server")
            private$msgBox$server(input, output, session)
            private$dtw$server(input, output, session)

            observe({
               mpg.max <- input$maxMpgSlider
               disp.max <- input$maxDispSlider
               tbl.sub <- subset(mtcars, mpg <= mpg.max & disp <= disp.max)
               printf("tbl.sub, %d rows", nrow(tbl.sub))
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
#x <- shinyApp(app$ui, app$server)a
runApp(shinyApp(app$ui, app$server), port=1112)

