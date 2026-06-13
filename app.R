library(shiny)

ui <- fluidPage(
  titlePanel("Next Word Predictor"),
  textInput("txt", "Enter text"),
  h3(textOutput("pred"))
)

server <- function(input, output) {
  output$pred <- renderText({
    # Clean up the input text
    userInput <- tolower(trimws(input$txt))
    
    # If the user hasn't typed anything yet, show nothing
    if (userInput == "") {
      return("")
    }
    
    # Look for matching bigrams using the variables currently in your environment
    match_pattern <- paste0("^", userInput, " ")
    bigram_names <- names(bigramFreq)
    candidates <- bigram_names[grep(match_pattern, bigram_names)]
    
    # If a match is found, return the prediction, otherwise fallback to "the"
    if (length(candidates) > 0) {
      prediction <- strsplit(candidates[1], " ")[[1]][2]
      return(prediction)
    } else {
      return("the")
    }
  })
}

shinyApp(ui, server)