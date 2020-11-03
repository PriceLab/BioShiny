#' R6 Class representing a simple text display widget
#'
#' @import R6
#' @import GetoptLong
#' @import grid
#' @import ComplexHeatmap
#'
#' @title HeatmapWidget
#' @description an R6 class providing clean access to a simple html message box
#' @name HeatmapWidget
#'
printf <- function(...) print(noquote(sprintf(...)))

library(R6)
library(grid)
library(ComplexHeatmap)

#' @export
HeatmapWidget = R6Class("HeatmapWidget",

    private = list(
        id = NULL,
        title = NULL,
        mtx = NULL,
        width = NULL,
        height = NULL,
        quiet = NULL,
        input = NULL,
        output = NULL,
        session = NULL,
        ht_list = NULL,
        shiny_env = NULL
        ),

        #' @description
        #' Create a new HeatmapWidget
        #' @param id the html document div id
        #' @param title character
        #' @param mtx a numeric matrix
        #' @param width numeric, pixels, default 600
        #' @param height numeric, pixels, default 600
        #' @param quiet logical silent or verbose?  default TRUE
        #'
        #' @return A new `HeatmapWidget` object.

   public = list(
       initialize = function(id, title, mtx, width=600, height=600, quiet=TRUE){

          if(!quiet) message("entering HeatmapWidget::initialize")
          private$id = id;
          private$title = title;
          private$mtx = mtx;
          private$width = width;
          private$height = height;
          private$quiet = quiet
          private$ht_list = Heatmap(as.matrix(mtx), name="title")
          private$shiny_env = new.env()
          },

        #' @description
        #' translates a mouse click into usable information
        #' @returns useful information
        #  @export

      clickEventAsPosition = function(event){
          pos1 = ComplexHeatmap:::get_pos_from_click(event)
          private$ht_list = private$shiny_env$ht_list
          pos = selectPosition(private$ht_list, mark = FALSE, pos = pos1, verbose = FALSE,
                               ht_pos = private$shiny_env$ht_pos)
          return(pos)
          },

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
          fluidPage(
              tags$head(
                   tags$script(HTML(
                            '$(function(){
                                 $("#heatmap_wrap").resizable({
                                    stop: function( event, ui ){
                                      document.getElementById("mask").remove();
                                      $("#heatmap_brush").remove();
                                      $("#heatmap").height(ui.size.height-4);
                                      $("#heatmap").width(ui.size.width-4);
                                      },
                                    start: function(event, ui) {
                                      var mask = document.createElement("div");
                                      mask.setAttribute("style", "position:absolute;top:0;background-color:rgba(255, 255, 0, 0.5)");
                                      mask.setAttribute("id", "mask");
                                      $("#heatmap_wrap").append(mask);
                                      },
                                    resize: function(event, ui) {
                                      $("#mask").width(ui.size.width);
                                      $("#mask").height(ui.size.height);
                                      }
                                   }); // heatmap_wrap
                                 $("#sub_heatmap_wrap").resizable({
                                    stop: function( event, ui ) {
                                       document.getElementById("mask2").remove();
                                       $("#sub_heatmap").height(ui.size.height-4);
                                       $("#sub_heatmap").width(ui.size.width-4);
                                       },
                                    start: function(event, ui){
                                       var mask = document.createElement("div");
                                       mask.setAttribute("style", "position:absolute;top:0;background-color:rgba(255, 255, 0, 0.5)");
                                       mask.setAttribute("id", "mask2");
                                       $("#sub_heatmap_wrap").append(mask);
                                       },
                                    resize: function(event, ui) {
                                       $("#mask2").width(ui.size.width);
                                       $("#mask2").height(ui.size.height);
                                       }
                                    }); // sub_heatmap_wrap
                                }); // function
                             ')),
                    tags$style(
                        "#heatmap_wrap, #sub_heatmap_wrap {
                           float:left;
                           margin-bottom: 10px;
                           }
                        #heatmap_wrap {
                          margin-right: 10px;
                          }
                        #heatmap, #sub_heatmap {
                          display: block;
                          margin: auto;
                          margin: auto;
                          }
                        ") # tags$style
                    ), # tags$head
          #div(plotOutput("heatmap", height = 346, width = 396,
          div(plotOutput("heatmap", height = private$height, width = private$width,
                         brush = "heatmap_brush",
                         click = "heatmap_click"
                         ),
              style = "position:relative;border:1px solid grey;",
              #style = "width:400px;height:350px;position:relative;border:1px solid grey;",
              id = "heatmap_wrap"
              ),
          div(plotOutput("sub_heatmap", height=private$height, width=private$width),
              style = "position:relative;border:1px solid grey;",
              #style = "width:300px;height:300px;position:relative;border:1px solid grey;",
              id = "sub_heatmap_wrap"
              ),
          div(style = "clear: both;"),
          #htmlOutput("click_info")
          ) # fluidPage
          }, # ui

        #' @description
        #' defines the event handlers used with the ui
        #' @param input list, managed by Shiny
        #' @param output list, managed by Shiny
        #' @param session list (environment?) managed by Shiny
        #' @returns nothing
      server = function(input, output, session){
         private$session = session;
         private$input = input
         private$output = output

         output$heatmap = renderPlot({
             width = session$clientData$output_heatmap_width
             height = session$clientData$output_heatmap_height

             #showNotification("Making the original heatmap.", duration = 5, type = "message")

             private$shiny_env$ht_list = draw(private$ht_list)
             private$shiny_env$ht_pos = ht_pos_on_device(private$shiny_env$ht_list)

             if(!private$quiet)
                 message(qq("[@{Sys.time()}] make the original heatmap and calculate positions (device size: @{width}x@{height} px)."))
             })

        # default
         output$sub_heatmap = renderPlot({
             grid.newpage()
             grid.text("No area on the heatmap is selected.", 0.5, 0.5)
              })

         output$click_info = renderUI({
             HTML("<pre>Not selected.</pre>")
             })

         observeEvent(input$heatmap_brush, {
             output$sub_heatmap = renderPlot({
                 width = session$clientData$output_sub_heatmap_width
                 height = session$clientData$output_sub_heatmap_height

                 if(is.null(input$heatmap_brush)) {
                     grid.newpage()
                     grid.text("No area on the heatmap is selected.", 0.5, 0.5)
                 } else {
                     #showNotification("Making the selected sub-heatmap.", duration = 1, type = "message")
                     lt = ComplexHeatmap:::get_pos_from_brush(input$heatmap_brush)
                     pos1 = lt[[1]]
                     pos2 = lt[[2]]

                     private$ht_list = private$shiny_env$ht_list
                     selected = selectArea(private$ht_list, mark = FALSE, pos1 = pos1, pos2 = pos2, verbose = FALSE, ht_pos = private$shiny_env$ht_pos)
                     private$shiny_env$selected = selected

                     if(is.null(selected)) {
                         grid.newpage()
                         grid.text("Selected area should overlap to heatmap bodies", 0.5, 0.5)
                     } else {

                         all_ht_name = unique(selected$heatmap)

                         ht_select = NULL
                         for(ht_name in all_ht_name) {
                             selected_current = selected[selected$heatmap == ht_name, ]
                             l1 = !duplicated(selected_current$row_slice)
                             rlt = selected_current$row_index[l1]
                             l2 = !duplicated(selected_current$column_slice)
                             clt = selected_current$column_index[l2]

                             ri = unlist(rlt)
                             ci = unlist(clt)
                             rs = rep(seq_along(rlt), times = sapply(rlt, length))
                             cs = rep(seq_along(clt), times = sapply(clt, length))
                             if(length(rlt) == 1) rs = NULL
                             if(length(clt) == 1) cs = NULL

                             ht_current_full = private$ht_list@ht_list[[ht_name]]
                             m = ht_current_full@matrix
                             subm = m[ri, ci, drop = FALSE]

                             ht_current = Heatmap(subm,
                                                  row_split = rs, column_split = cs,
                                                  col = ht_current_full@matrix_color_mapping,
                                                  show_heatmap_legend = FALSE,
                                                  cluster_rows = FALSE, cluster_columns = FALSE,
                                                  row_title = NULL, column_title = NULL,
                                                  border = ht_current_full@matrix_param$border
                                                  )

                             if(private$ht_list@direction == "horizontal") {
                                 ht_select = ht_select + ht_current

                             } else {
                                 ht_select = ht_select %v% ht_current
                             }
                             draw(ht_select)
                         }

                         if(!private$quiet)
                            message(qq("[@{Sys.time()}] make the sub-heatmap (device size: @{width}x@{height} px)."))
                     }
                 }
             })

             output$click_info = renderUI({
                 selected = private$shiny_env$selected
                 #printf("--- new selection: ")
                 #print(selected)
                 if(is.null(selected)) {
                     HTML(paste("<pre>",
                                "Selected area should overlap to heatmap bodies.",
                                "</pre>",
                                sep = "\n"))
                 } else {
                     #print("a")
                     n_ht = length(unique(selected$heatmap))
                     #print("b")
                     private$ht_list = private$shiny_env$ht_list
                     #print("c")
                     if(private$ht_list@direction == "horizontal") {
                        #print("e")
                        l1 = !duplicated(selected$row_slice)
                        nr = length(unlist(selected$row_index[l1]))
                        l2 = !duplicated(paste0(selected$heatmap, selected$column_slice))
                        nc = length(unlist(selected$column_index[l2]))
                        HTML("horizontal direction")
                     } else {
                        #print("g")
                        l1 = !duplicated(paste0(selected$heatmap, selected$row_slice))
                        nr = length(unlist(selected$row_index[l1]))
                        #print("h")
                        l2 = !duplicated(selected$column_slice)
                        nc = length(unlist(selected$column_index[l2]))
                        HTML("not horizontal direction")
                        #print("i")
                        }

                     #selected_df = as.data.frame(selected)
                     #con = textConnection("dump_txt", "w")
                     #dump("selected_df", file = con)
                     #close(con)
                     #dump_txt = dump_txt[-1]
                     #dump_txt = paste(dump_txt, collapse = "\n")
                     #HTML(paste("<pre>",
                     #           qq("Selected over @{n_ht} heatmap@{ifelse(n_ht > 1, 's', '')} with @{nr} row@{ifelse(nr > 1, 's', '')} and @{nc} column@{ifelse(nc > 1, 's', '')}"),
                     #           "You can get the row and column indices by copying following code: ",
                     #           "</pre>",
                     #           "<p><input id='show_code' type='button' value='show/hide code' /></p>",
                     #           "<pre id='code'>",
                     #           dump_txt,
                     #           "</pre>",
                     #           "<script>",
                     #           "$('#code').hide();",
                     #           "$('#show_code').click(function(){ $('#code').toggle(); });",
                     #           "</script>",
                     #sep = "\n"))
                 } # else: something seleted
             })

         })

      } # server

     ) # public
  ) # class

