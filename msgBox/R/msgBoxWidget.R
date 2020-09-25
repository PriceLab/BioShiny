#' R6 Class representing a simple text display widget
#'
#' @title msgBoxWidget
#' @description an R6 class providing clean access to a simple html message box
#' @name msgBoxWidget
#'
library(R6)

#' @export
msgBoxWidget = R6Class("msgBoxWidget",

    private = list(
        id = NULL,
        title = NULL,
        boxWidth = NULL,
        boxHeight = NULL,
        fontSize = NULL,
        fontColor = NULL,
        backgroundColor = NULL,
        session = NULL,
        input = NULL,
        output = NULL
        ),

        #' @description
        #' Create a new msgBoxWidget
        #' @param id the html document div id
        #' @param title character
        #' @param boxWidth integer, 200 by default
        #' @param boxHeight integer, 30 by default
        #' @param fontSize integer, 20 by default
        #' @param fontColor character, standard CSS rgb or name
        #' @param backgroundColor character, standard CSS rgb or name
        #' @return A new `msgBoxWidget` object.

   public = list(

       initialize = function(id, title, boxWidth=200, boxHeight=30, fontSize=20,
                             fontColor="black", backgroundColor="beige"){

          printf("entering msgBoxWidget::initialize")
          private$id = id;
          private$title = title;
          private$boxWidth = boxWidth;
          private$boxHeight = boxHeight;
          private$fontSize = fontSize;
          private$fontColor = fontColor;
          private$backgroundColor = backgroundColor;
          },

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
          fluidRow(
              div(
                div(tags$label(private$title), style="margin-left: 10px;"),
                div(htmlOutput(outputId=private$id,
                   style=sprintf("background-color: %s; padding-left: 10px; width: %dpx; height: %dpx; font-size:%dpx; color: %s",
                                               private$backgroundColor, private$boxWidth, private$boxHeight,
                                               private$fontSize, private$fontColor)),
                      style=sprintf("margin-left: 5px; padding=10px; border: 1px solid gray; width: %dpx;",
                                    private$boxWidth+2))
              ) # div
              ) # fluidRow
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
         }, # server

      setText = function(newText){
         private$output[[private$id]] <- renderText(newText)
         }

     ) # public
  ) # class

