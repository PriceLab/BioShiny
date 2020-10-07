printf <- function(...) print(noquote(sprintf(...)))

#' R6 Class representing a simple text display widget
#'
#' @title TextInputWidget
#' @description an R6 class providing clean access to a simple html message box
#' @name TextInputWidget
#'
library(R6)

#' @export
TextInputWidget = R6Class("TextInputWidget",

    private = list(
        id = NULL,
        title = NULL,
        initialValue = NULL,
        placeholder = NULL,
        boxWidth = NULL,
        boxHeight = NULL,
        fontSize = NULL,
        fontColor = NULL,
        backgroundColor = NULL,
        session = NULL,
        input = NULL,
        output = NULL,
        currentText = NULL
        ),

        #' @description
        #' Create a new TextInputWidget
        #' @param id the html document div id
        #' @param title character
        #' @param initialValue character, default ""
        #' @param placeholder character, provides hint to user
        #' @param boxWidth integer, 200 by default
        #' @param boxHeight integer, 30 by default
        #' @param fontSize integer, 20 by default
        #' @param fontColor character, standard CSS rgb or name
        #' @param backgroundColor character, standard CSS rgb or name
        #' @return A new `TextInputWidget` object.

   public = list(

       initialize = function(id, title, initialValue="", placeholder="", boxWidth=200, boxHeight=30,
                             fontSize=20, fontColor="black", backgroundColor="beige"){

          printf("entering TextInputWidget::initialize")
          private$id = id;
          private$title = title;
          private$initialValue = initialValue;
          private$placeholder = placeholder;
          private$title = title;
          private$boxWidth = boxWidth;
          private$boxHeight = boxHeight;
          private$fontSize = fontSize;
          private$fontColor = fontColor;
          private$backgroundColor = backgroundColor;
          private$currentText = private$initialValue;
          },

        #' @description
        #' defines the html structure of this widget
        #' @returns shiny code which, wnen invoked (almost always by the shinyApp function, returns html
      ui = function(){
          textInput(inputId=private$id, label=private$title, value=private$initialValue,
                    placeholder=private$placeholder)
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

         observeEvent(input[[private$id]], ignoreInit=TRUE, {
            private$currentText <- input[[private$id]]
            })
         }, # server

      setText = function(newText){
         updateTextInput(private$session, inputId=private$id, label=NULL, value=newText)
         },

      getText = function(newText){
         private$currentText
         }

     ) # public
  ) # class

