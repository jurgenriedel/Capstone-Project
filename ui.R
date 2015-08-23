#install.packages("BH")
require(BH)
require(shiny)
shinyUI(fluidPage(
  
  headerPanel("Next Word Prediction"), 
  img(src="wordcloud.png", height = 200, width = 400),
  br(),
    tabsetPanel(selected = "Prediction",
    tabPanel("Prediction",                             
        textInput('sentence', 'Enter a sentence:'),
        tags$head(tags$style(type="text/css", "#sentence {width: 600px}")),
        radioButtons("model", "Select model:",
                     c("back-off" = "0",
                       "interpolation" = "1")),
        submitButton("Predict"),
        h4('Predicted words in descending probability: '),
        verbatimTextOutput("result")
    ),
    tabPanel("Model decription",
             h4("The data for the app "),
             p("The data for the ",
               strong("Capstone Project "),   
                "were downloaded from the course website ( original site: ", 
               span("http://www.corpora.heliohost.org", style = "color:blue"), 
               "). The dataset contains text files of news & blog entires as well as tweets in English version.", 
               style = "font-family: 'arial'"),
             
             h4("Sample size "),
             p("The center of the analysis is the corpus. We are using for this project the corpus data structure,
                provided by the text mining framework library, tm. Since the text documents are rather large a  
                sample is drawn of each text containing ca. 2 Mio characters. Since this is ca. 10% of the number of total 
                characters in each text file, we should have a statistical significant sample size.", 
                style = "font-family: 'arial'"),
             
             h4("The model "),
             p("The predictive model is based on N-gram up to a size of 4. The model is dealing with unkown N-grams by using a closed vocabulary.
                The simplest version of the model implements relative probabilities with a simple back-off algorithm. A more sophisticated model 
                is us using Good-Turing smoothing with interpolation and a Katz-style back-off algorithm.
               ", 
               style = "font-family: 'arial'"),
             h4("Evaluation "),
             p("The performance of different models were evaluated with the intrinsic perplexity measure 
                rather than explicitly with statistical methods. The sample corpus was devided into a training set, 
                a hold out data set, and a test set. The hold out data set was used to fine-tune weighting factor for the back-off algorithm.
               ", 
               style = "font-family: 'arial'"),
             h4("Slide deck of project "),
             span("http://rpubs.com/jurgenriedel/wp_app", style = "color:blue"), 
             helpText(   a("Click here",     href="http://rpubs.com/jurgenriedel/wp_app")
             ),             br(),
             h4("Source code of project "),
             span("https://github.com/jurgenriedel/Capstone-Project", style = "color:blue"),
             helpText(   a("Click here",     href="https://github.com/jurgenriedel/Capstone-Project")
             )
             
             
             
             
             

    )
    ),
    mainPanel(
#       h4('Predicted words in descending probability: '),
#         verbatimTextOutput("result")
    )
))