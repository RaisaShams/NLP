library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title="Analysis Team 9"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Hult-ComboMenu", icon = icon("th"), tabName = "widgets"),
      menuItem("Histogram", icon = icon("bar-chart-o"), startExpanded = TRUE,
               menuSubItem("Pizza topping preferences", tabName = "subitem1"),
               menuSubItem("How do you like your crust", tabName = "subitem2"),
               menuSubItem("Kind of beverage", tabName = "subitem3"),
               menuSubItem("Say yes to Pineapple", tabName = "subitem4"),
               menuSubItem("Health conscious", tabName = "subitem5")),
      menuItem("Sentiment", icon = icon("bar-chart-o"), startExpanded = TRUE,
               menuSubItem("Pizza topping preferences", tabName = "subitem6"),
               menuSubItem("How do you like your crust", tabName = "subitem7"),
               menuSubItem("Kind of beverage", tabName = "subitem8"),
               menuSubItem("Say yes to Pineapple", tabName = "subitem9"),
               menuSubItem("Health conscious", tabName = "subitem10")),
      
      menuItem("LDA", icon = icon("th"), tabName = "LDA"),
      
      menuItem("Network", icon = icon("th"), tabName = "Network" ,startExpanded = TRUE,
               menuSubItem("Pizza topping preferences", tabName = "subitem11"),
               menuSubItem("How do you like your crust", tabName = "subitem12"),
               menuSubItem("Kind of beverage", tabName = "subitem13"),
               menuSubItem("Say yes to Pineapple", tabName = "subitem14"),
               menuSubItem("Health conscious", tabName = "subitem15")),
      
      menuItem("TF-IDF", icon = icon("th"), tabName = "plottf_idf"),
      menuItem("WorldCloud", tabName = "worldcloud1", icon = icon("dashboard"))
    ),# closeSlider Menu
    textOutput("res")
  ),#close sidebar
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                box(title=" Prediction Model ",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("barchart", height =800, width=800)))),
      tabItem("widgets", "Widgets tab content"),
      
      tabItem("subitem1",
              fluidRow(
                box(title="Frequency",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("graph_1", height =500)),
                
                box(title = "Pizza Toppings",status = "warning",solidHeader = TRUE,
                    sliderInput("frequency", "Top Frequencies:", min=2, max=15, value=10)))),
      
      tabItem("subitem2",
              fluidRow(
                box(title="Frequency",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("graph_2", height =500)),
                
                box(title = "Most likely crust",status = "warning",solidHeader = TRUE,
                    sliderInput("frequency2", "Top Frequencies:", min=2, max=15, value=10)))),
      
      tabItem("subitem3",
              fluidRow(
                box(title="Frequency",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("graph_3", height =500)),
                
                box(title = "Prefered Beverages",status = "warning",solidHeader = TRUE,
                    sliderInput("frequency3", "Top Frequencies:", min=2, max=15, value=10)))),
      
      tabItem("subitem4", 
              fluidRow(
                box(title="Frequency",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("graph_4", height =500)),
                
                box(title = "Love for pineapple",status = "warning",solidHeader = TRUE,
                    sliderInput("frequency4", "Top Frequencies:", min=2, max=15, value=10)))),
      
      tabItem("subitem5",
              fluidRow(
                box(title="Frequency",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("graph_5", height =500)),
                
                box(title = "Health conscious???",status = "warning",solidHeader = TRUE,
                    sliderInput("frequency5", "Top Frequencies:", min=1, max=2, value=1)))),
      
      tabItem("subitem7", 
              fluidRow(
                box(title="Contribution to sentiment",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("plot7", height =500)),
              )),
      tabItem("subitem6", 
              fluidRow(
                box(title="Contribution to sentiment",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("plot8", height =500)),
              )),
      tabItem("subitem8", 
              fluidRow(
                box(title="Contribution to sentiment",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("plot9", height =500)),
              )),
      tabItem("subitem9",
              fluidRow(
                box(title="Contribution to sentiment",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("plot10", height =500)),
              )),
      tabItem("subitem10", "Sub-item 10 tab content"),
      
      tabItem("LDA",
              fluidRow(
                box(title="Linear Discriminant Analysis",solidHeader = TRUE,collapsible=TRUE,status = "primary",
                    plotOutput("plotlda", height =800, width=800)))),
      tabItem("subitem11",
              fluidRow(
                box(plotOutput("plot1", height =800 , width = 800)))),
      tabItem("subitem12",
              fluidRow(
                box(plotOutput("plot2", height =800 , width = 800)))),
      tabItem("subitem13",
              fluidRow(
                box(plotOutput("plot3", height =800 , width = 800)))),
      tabItem("subitem14",
              fluidRow(
                box(plotOutput("plot4", height =800 , width = 800)))),
      tabItem("plottf_idf",
              fluidRow(
                box(plotOutput("plottf_idf", height =800, width=800)))),
      tabItem("worldcloud1",
              fluidRow(
                box(selectInput("selection", "Choose a Question:",
                                choices = questions),
                    actionButton("update", "Change"),
                    
                    sliderInput("freq",
                                "Minimum Frequency:",
                                min = 1,  max = 50, value = 15),
                    sliderInput("max",
                                "Maximum Number of Words:",
                                min = 1,  max = 200,  value = 100),
                    mainPanel(plotOutput("worldcloud1")))))
    )#close DashboardBody
  )#close 
)#close DashboardPage

server <- function(input, output, session) {
  output$res <- renderText({
    req(input$sidebarItemExpanded)
    paste("Expanded menuItem:", input$sidebarItemExpanded)
  })
  library(pdftools)
  library(shapeR)
  library(tidytext)
  library(tidyverse)
  library(textreadr)
  library(textshape)
  library(dplyr)
  library(scales)
  library(tidyr)
  library(dplyr)
  library(tidytext)
  library(stringr)
  library(ggplot2)
  library(textdata)
  library(reshape2)
  library(ggraph)
  
  team_pdf_text <- read_document(file="C:\\Users\\raisa\\Desktop\\MBAN\\Spring\\Text Analytics\\Group Assignment\\TEAM 9\\Team 9 Final File.docx")
  team_project <- c(team_pdf_text)
  a <- 47
  b <- 5
  team_pdf_text <- as.data.frame(matrix(nrow=a, ncol=b))
  for(z in 1:b){
    for(i in 1:a){
      team_pdf_text[i,z]<- team_project[i*b+z-b]}}
  cust_stop <- data_frame(
    word=c("1","2","3","4","5","drink","person","depends","I","like","the","don't", "am", "really", "can't", "usually", "don't", "guess","i'm","it's","i'm","lot","don't","belong","pizza",
           "don't","it's","pretty","people","that's","it's"),
    lexicon=rep("custom", each=31)
  ) #closing data_frame
  
  Q1 <- team_pdf_text$V1
  Q1_df <- data_frame(line=1:a, text=Q1)
  
  Q2 <- team_pdf_text$V2
  Q2_df <- data_frame(line=1:a, text=Q2)
  
  Q3 <- team_pdf_text$V3
  Q3_df <- data_frame(line=1:a, text=Q3)
  
  Q4 <- team_pdf_text$V4
  Q4_df <- data_frame(line=1:a, text=Q4)
  
  Q5 <- team_pdf_text$V5
  Q5_df <- data_frame(line=1:a, text=Q5)
  
  tidy_Q1 <- Q1_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)%>%
    anti_join(cust_stop)%>%
    count(word, sort=TRUE)
  
  tidy_Q2 <- Q2_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)%>%
    anti_join(cust_stop)%>%
    count(word, sort=TRUE)
  
  tidy_Q3 <- Q3_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)%>%
    anti_join(cust_stop)%>%
    count(word, sort=TRUE)
  
  tidy_Q4<- Q4_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)%>%
    anti_join(cust_stop)%>%
    count(word, sort=TRUE)
  
  tidy_Q5 <- Q5_df %>%
    unnest_tokens(word, text) %>%
    count(word, sort=TRUE)
  
  output$graph_1 <- renderPlot({
    Q1_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% 
      anti_join(cust_stop) %>%
      count(word, sort=TRUE) %>%
      top_n(input$frequency)%>%
      mutate(word = reorder(word,n)) %>%
      ggplot(aes(word, n))+
      geom_col()+
      xlab(NULL)+
      coord_flip()
  })
  
  output$graph_2 <- renderPlot({
    Q2_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% 
      anti_join(cust_stop) %>%
      count(word, sort=TRUE) %>%
      top_n(input$frequency2)%>%
      mutate(word = reorder(word,n)) %>%
      ggplot(aes(word, n))+
      geom_col()+
      xlab(NULL)+
      coord_flip()
  })
  
  output$graph_3 <- renderPlot({
    Q3_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% 
      anti_join(cust_stop) %>%
      count(word, sort=TRUE) %>%
      top_n(input$frequency3)%>%
      mutate(word = reorder(word,n)) %>%
      ggplot(aes(word, n))+
      geom_col()+
      xlab(NULL)+
      coord_flip()
  })
  
  output$graph_4 <- renderPlot({
    Q4_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% 
      anti_join(cust_stop) %>%
      count(word, sort=TRUE) %>%
      top_n(input$frequency4)%>%
      mutate(word = reorder(word,n)) %>%
      ggplot(aes(word, n))+
      geom_col()+
      xlab(NULL)+
      coord_flip()
  })
  
  output$graph_5 <- renderPlot({
    Q5_df %>%
      unnest_tokens(word, text) %>%
      count(word, sort=TRUE) %>%
      top_n(input$frequency5)%>%
      mutate(word = reorder(word,n)) %>%
      ggplot(aes(word, n))+
      geom_col()+
      xlab(NULL)+
      coord_flip()
  })
  ####  LDA 
  questions_mutate <- bind_rows(
    mutate(tidy_Q1, question = 'Q1'),
    mutate(tidy_Q2, question = 'Q2'),
    mutate(tidy_Q3, question = 'Q3'),
    mutate(tidy_Q4, question = 'Q4'),
    mutate(tidy_Q5, question = 'Q5'))
  
  output$plotlda <- renderPlot({
    questions_mutate%>%
      cast_dtm(question, word, n )
    n <- questions_mutate %>%
      cast_sparse(question, word, n)
    class(n)
    dim(n)
    pizza_LDA <- LDA(n, k=3, control = list(seed=123)) 
    ap_topics <- tidy(pizza_LDA, matrix="beta")  
    top_terms <- ap_topics %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)
    top_terms %>%
      mutate(term=reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend=FALSE) +
      facet_wrap(~topic, scales = "free") +
      coord_flip()
  })
  #######  NETWORK ##### 
  output$plot1 <- renderPlot({
    my_bigrams1 <- Q1_df %>%
      unnest_tokens(bigram, text, token = "ngrams", n=2)
    bigrams_separated1 <- my_bigrams1 %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    bigrams_filtered1 <- bigrams_separated1 %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(!word1 %in% cust_stop$word) %>%
      filter(!word2 %in% cust_stop$word)
    
    bigram_counts1 <- bigrams_filtered1 %>%
      count(word1, word2, sort = TRUE)
    
    bigram_graph1 <- bigram_counts1 %>%
      filter(n > 0) 
    ggraph(bigram_graph1, layout = "fr") +
      geom_edge_link(colour = "tan1") +
      geom_node_point(size = 5, color = "slategray") +
      scale_edge_width(range = c(0.2, 2)) + 
      geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines"))+
      theme_graph()
  })  
  
  output$plot2 <- renderPlot({
    my_bigrams2 <- Q2_df %>%
      unnest_tokens(bigram, text, token = "ngrams", n=2)
    
    bigrams_separated2 <- my_bigrams2 %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered2 <- bigrams_separated2 %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(!word1 %in% cust_stop$word) %>%
      filter(!word2 %in% cust_stop$word)
    
    bigram_counts2 <- bigrams_filtered2 %>%
      count(word1, word2, sort = TRUE)
    
    bigram_graph2 <- bigram_counts2 %>%
      filter(n > 0)
    
    ggraph(bigram_graph2, layout = "fr") +
      geom_edge_link(colour = "tan1") +
      geom_node_point(size = 5, color = "slategray") +
      scale_edge_width(range = c(0.2, 2)) + 
      geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines"))+
      theme_graph()
    
  })
  
  output$plot3 <- renderPlot({
    my_bigrams3 <- Q3_df %>%
      unnest_tokens(bigram, text, token = "ngrams", n=2)
    
    bigrams_separated3 <- my_bigrams3 %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered3 <- bigrams_separated3 %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(!word1 %in% cust_stop$word) %>%
      filter(!word2 %in% cust_stop$word)
    
    bigram_counts3 <- bigrams_filtered3 %>%
      count(word1, word2, sort = TRUE)
    
    bigram_graph3 <- bigram_counts3 %>%
      filter(n > 0)
    
    ggraph(bigram_graph3, layout = "fr") +
      geom_edge_link(colour = "tan1") +
      geom_node_point(size = 4, color = "slategray") +
      scale_edge_width(range = c(0.2, 2)) + 
      geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines"))+
      theme_graph()
    
  })
  
  output$plot4 <- renderPlot({
    my_bigrams4 <- Q4_df %>%
      unnest_tokens(bigram, text, token = "ngrams", n=2)
    
    bigrams_separated4 <- my_bigrams4 %>%
      separate(bigram, c("word1", "word2"), sep = " ")
    
    bigrams_filtered4 <- bigrams_separated4 %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>%
      filter(!word1 %in% cust_stop$word) %>%
      filter(!word2 %in% cust_stop$word)
    
    bigram_counts4 <- bigrams_filtered4 %>%
      count(word1, word2, sort = TRUE)
    
    bigram_graph4 <- bigram_counts4 %>%
      filter(n > 0)
    
    ggraph(bigram_graph4, layout = "fr") +
      geom_edge_link(colour = "tan1") +
      geom_node_point(size = 4, color = "slategray") +
      scale_edge_width(range = c(0.2, 2)) + 
      geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines"))+
      theme_graph()
  })
  ### SENTIMENT     
  output$plot7 <- renderPlot({
    tidy_Q2 <- Q2_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      anti_join(cust_stop) %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort=T) 
    tidy_Q2 %>%
      group_by(sentiment) %>%
      #top_n(input$Frequency) %>%
      ungroup() %>%
      mutate(word=reorder(word, n)) %>%
      ggplot(aes(word, n, fill=sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y")+
      labs(y="Contribution to sentiment", x=NULL)+
      coord_flip()
  })
  
  output$plot8 <- renderPlot({
    tidy_Q1 <- Q1_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      anti_join(cust_stop) %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort=T) 
    tidy_Q1 %>%
      group_by(sentiment) %>%
      #top_n(input$Frequency) %>%
      ungroup() %>%
      mutate(word=reorder(word, n)) %>%
      ggplot(aes(word, n, fill=sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y")+
      labs(y="Contribution to sentiment", x=NULL)+
      coord_flip()
  })
  
  output$plot9 <- renderPlot({
    tidy_Q3 <- Q3_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      anti_join(cust_stop) %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort=T) 
    tidy_Q3 %>%
      group_by(sentiment) %>%
      #top_n(input$Frequency) %>%
      ungroup() %>%
      mutate(word=reorder(word, n)) %>%
      ggplot(aes(word, n, fill=sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y")+
      labs(y="Contribution to sentiment", x=NULL)+
      coord_flip()
  })
  output$plot10 <- renderPlot({
    tidy_Q4 <- Q4_df %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      anti_join(cust_stop) %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort=T) 
    tidy_Q4 %>%
      group_by(sentiment) %>%
      #top_n(input$Frequency) %>%
      ungroup() %>%
      mutate(word=reorder(word, n)) %>%
      ggplot(aes(word, n, fill=sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y")+
      labs(y="Contribution to sentiment", x=NULL)+
      coord_flip()
  })
  
  
  
  ########TF- IDF ##### 
  output$plottf_idf <- renderPlot({  
    questions_mutate <- bind_rows(
      mutate(tidy_Q1, question = 'Q1'),
      mutate(tidy_Q2, question = 'Q2'),
      mutate(tidy_Q3, question = 'Q3'),
      mutate(tidy_Q4, question = 'Q4'),
      mutate(tidy_Q5, question = 'Q5'),)
    
    questions_mutate <- questions_mutate %>%
      bind_tf_idf(word, question, n) 
    questions_mutate
    questions_mutate %>%
      arrange(desc(tf_idf))
    
    questions_mutate %>%
      arrange(desc(tf_idf)) %>%
      mutate(word=factor(word, levels=rev(unique(word)))) %>%
      group_by(question) %>%
      top_n(5) %>%
      ungroup %>%
      ggplot(aes(word, tf_idf, fill=question))+
      geom_col(show.legend=FALSE)+
      labs(x=NULL, y="tf-idf")+
      facet_wrap(~question, ncol=2, scales="free")+
      coord_flip()
  })  
  ####BAR CHART 
  output$barchart <- renderPlot({    
    counts <- table(team_pdf_text$V5)
    barplot(counts, main="Healthy Vs Unhealthy",
            xlab="Healthy or not ?", col=c("darkblue","green"),
            legend = rownames(counts), beside=TRUE) 
  })
  ###### WORLDCLOUD  ###
  library(wordcloud)
  output$worldcloud1 <- renderPlot({  
    Q1_df %>%
      unnest_tokens(word,text)%>%
      anti_join(cust_stop)%>%
      anti_join(cust_stop)%>%
      mutate(linenumber=row_number())%>%
      anti_join(stop_words)%>%
      count(word,sort=TRUE)%>%
      with(wordcloud(word,n,
                     max.words=input$max,
                     min.freq = input$freq),
           colors=brewer.pal(8,"Dark2"))
    
    terms <- reactive({
      input$update
      isolate({ })  })
    v <- terms()
  })
}#close 

shinyApp(ui,server)