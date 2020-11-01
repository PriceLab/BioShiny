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
        #' @return A new `HeatmapWidget` object.

   public = list(
       initialize = function(id, title, mtx){

          printf("entering HeatmapWidget::initialize")
          private$id = id;
          private$title = title;
          private$mtx = mtx;
          private$ht_list = Heatmap(as.matrix(mtx), name="fubar")
          private$shiny_env = new.env()
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
                    # tags$style(paste(readLines(system.file("app", "jquery-ui.css", package = "ComplexHeatmap")), collapse = "\n")),
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
          div(plotOutput("heatmap", height = 346, width = 396,
                         brush = "heatmap_brush",
                         click = "heatmap_click"
                         ),
              style = "width:400px;height:350px;position:relative;border:1px solid grey;",
                id = "heatmap_wrap"
              ),
          div(plotOutput("sub_heatmap", height = 296, width = 296),
              style = "width:300px;height:300px;position:relative;;border:1px solid grey;",
              id = "sub_heatmap_wrap"
              ),
          div(style = "clear: both;"),
          htmlOutput("click_info")
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

             showNotification("Making the original heatmap.", duration = 1, type = "message")

             private$shiny_env$ht_list = draw(private$ht_list)
             private$shiny_env$ht_pos = ht_pos_on_device(private$shiny_env$ht_list)

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
                     showNotification("Making the selected sub-heatmap.", duration = 1, type = "message")
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

                         message(qq("[@{Sys.time()}] make the sub-heatmap (device size: @{width}x@{height} px)."))
                     }
                 }
             })

             output$click_info = renderUI({
                 selected = private$shiny_env$selected
                 printf("--- new selection: ")
                 print(selected)
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

         observeEvent(input$heatmap_click, {
             output$click_info = renderUI({
                 showNotification("Click on the heatmap.", duration = 1, type = "message")
                 pos1 = ComplexHeatmap:::get_pos_from_click(input$heatmap_click)
                 #print(1)
                 private$ht_list = private$shiny_env$ht_list
                 #print(2)
                 pos = selectPosition(private$ht_list, mark = FALSE, pos = pos1, verbose = FALSE, ht_pos = private$shiny_env$ht_pos)
                 #print(3)
                 if(is.null(pos)) {
                     HTML(paste("<pre>",
                                "You did not select inside the heatmap.",
                                "</pre>",
                                sep = "\n"))
                 } else {
                     #print(4)
                     ht_name = pos[1, "heatmap"]
                     #print(5)
                     slice_name = pos[1, "slice"]
                     #print(6)

                     row_index = pos[1, "row_index"][[1]]
                     #print(7)
                     column_index = pos[1, "column_index"][[1]]
                     #print(8)
                     m = private$ht_list@ht_list[[ht_name]]@matrix
                     #print(9)
                     v = m[row_index, column_index]
                     #print(10)
                     col = map_to_colors(private$ht_list@ht_list[[ht_name]]@matrix_color_mapping, v)
                     #print(12)
                     row_label = rownames(m)[row_index]
                     #print(13)
                     column_label = colnames(m)[column_index]
                     #print(14)
                     if(is.null(row_label)) {
                         row_label = "NULL"
                     } else {
                         # row_label = paste0("'", row_label, "'")
                     }
                     if(is.null(column_label)) {
                         column_label = "NULL"
                     } else {
                         # column_label = paste0("'", column_label, "'")
                     }

                     #print(4)
                     message(qq("[@{Sys.time()}] click on the heatmap @{slice_name}."))

                     HTML(paste("<pre>",
                                qq("heatmap: @{ht_name}"),
                                qq("heatmap slice: @{slice_name}"),
                                qq("row index: @{row_index}"),
                                qq("row label: @{row_label}"),
                                qq("column index: @{column_index}"),
                                qq("column_label: @{column_label}"),
                                qq("value: @{v} <span style='background-color:@{col};width=10px;'>    </span>"),
                                "</pre>",
                                sep = "\n"))
                 } # else
                }) # renderUI
        }) # observeEvent
      } # server

     ) # public
  ) # class

