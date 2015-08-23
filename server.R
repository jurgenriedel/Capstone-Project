library(shiny)
library(UsingR)

shinyServer(
    function(input, output) {
        next.word <- reactive({
            if (nchar(str_trim(input$sentence))>0){
              sentence <- PrepareInput(input$sentence)
              df <- data.frame(strsplit(sentence," "))
              n <- nrow(df)
              if (n>3){
                sentence <- paste(df[(n-2):n,1], collapse = " ")
              }
              if (input$model==0){
                  next.word.list <- PredictNextWord(sentence,3,0,10)                  
              } else {
                  next.word.list <- PredictNextWord(sentence,3,1,10)                  
              }
              as.character(paste0(next.word.list$token,collapse= ", "))
            }
        })
        
        output$result <- renderText(next.word())
    }
)