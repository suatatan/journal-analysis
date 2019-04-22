library(shiny)
ui <- fluidPage(
   titlePanel("JournalAnalytics (Dr.Suat ATAN)"),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         #textInput("dergi","Journal",value="emerald_insight.csv"),
         selectInput("dergi", label = h3("Select box"), 
                     choices = list("Emerald Insight" = "emerald_insight.csv", "Social Networks" = "social_networks.csv")),
         textInput("sene","Year (0 reflects all years)",value="0"),
         textInput("kelimeler","Keywords",placeholder = "Virgulle ayirin",value="futur,technologi,foresight")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(type="tabs",
                     tabPanel("AllYears",DT::dataTableOutput("tum_yillar")),
                     tabPanel("WordFreq",DT::dataTableOutput("word_freq")),
                     tabPanel("Bigrams",DT::dataTableOutput("bigram_counts")),
                     tabPanel("Co-occurence",DT::dataTableOutput("word_pairs")),
                     tabPanel("Sankey",imageOutput("sankey"))
                     )
      )
   )
)

#-------------------------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
   dataInput <-reactive({
     
     #Adim 1
     library(tidytext)
     library(dplyr)
     library(stringr)
     library(SnowballC)
     library(readr)
     library(readr)
     library(DT)
     library(tidyr)
     library(widyr)
     library(alluvial)
     library(stringr)
     journal <- read_delim(input$dergi, 
                                   ",", escape_double = FALSE, 
                                   trim_ws = TRUE)
     
     custom_stop_words <- read_delim("cikarilacak_kelimeler.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)
     custom_stop_words <- custom_stop_words %>% 
       mutate (word = wordStem(word, language="english"))
     #journal <- journal %>% mutate(tarih = substr(issue_date,12,15))
     journal <- journal %>% mutate(tarih = str_extract(issue_date,"(19|20)\\d{2}") )
     if(input$sene==0){
       journal <- journal
     }
     else{
       journal <- journal %>% filter(tarih == input$sene)
     }
     #tum yillar
     tum_yillar<-journal %>% group_by(tarih) %>% count()
     books <- journal %>% 
       select(`article-href`,abstract,issue_date)
     tidy_books <- books %>%
       unnest_tokens(word, abstract) %>%
       mutate (word = wordStem(word, language="english")) %>%
       anti_join(stop_words) %>%
       anti_join(custom_stop_words)
     #Most Frequent Words
     word_freq <- tidy_books %>%
       count(word, sort = TRUE) 
     #N-grams
     austen_bigrams <- books %>%
       unnest_tokens(bigram, abstract, token = "ngrams", n = 2)
     bigrams_separated <- austen_bigrams %>%
       separate(bigram, c("word1", "word2"), sep = " ")
     bigrams_filtered <- bigrams_separated %>%
       filter(!word1 %in% stop_words$word) %>%
       filter(!word2 %in% stop_words$word) %>%
       filter(!word1 %in% custom_stop_words$word) %>%
       filter(!word2 %in% custom_stop_words$word)
     # new bigram counts:
     bigram_counts <- bigrams_filtered %>% 
       count(word1, word2, sort = TRUE)
     # Co-occurence
     austen_section_words <- books %>%
       unnest_tokens(word, abstract) %>%
       filter(!word %in% stop_words$word) %>%
       filter(!word %in% custom_stop_words$word)
     austen_section_words <- austen_section_words %>% mutate(section=`article-href`)
     austen_section_words
     austen_section_words<-austen_section_words %>% mutate (word = wordStem(word, language="english"))
     # count words co-occuring within sections
     word_pairs <- austen_section_words %>%
       pairwise_count(word, section, sort = TRUE) %>%
       filter(!item1 %in% stop_words$word) %>%
       filter(!item2 %in% stop_words$word) %>%
       filter(!item1 %in% custom_stop_words$word) %>%
       filter(!item2 %in% custom_stop_words$word)
     
     
     #keywords <- c('futur','technologi','foresight','econom')
     keywords  <- unlist(strsplit(input$kelimeler, split=","))
     tq <- (word_pairs)
     tq <- tq %>% filter(item1 %in% keywords)
     tq <- head(tq,20)
     
     #returns
     
     list(word_freq=word_freq,
          tum_yillar = tum_yillar,
          bigram_counts = bigram_counts,
          word_pairs = word_pairs,
          keywords = keywords,
          tq = tq
          )
    
   })
   output$tum_yillar <- DT::renderDataTable({
     dataInput()$tum_yillar
   })
   output$word_freq <- DT::renderDataTable({
     dataInput()$word_freq
   })
   output$bigram_counts <- DT::renderDataTable({
     dataInput()$bigram_counts
   })
   output$word_pairs <- DT::renderDataTable({
     dataInput()$word_pairs
   })
   output$sankey <- renderPlot({
       alluvial(dataInput()$tq[,1:2],freq=dataInput()$tq$n,
                xw=0.2, alpha=0.2,
                cex=0.7,
                gap.width=0.1, col= "steelblue", border="white"
       )
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

