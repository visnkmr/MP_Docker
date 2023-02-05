
    # library(rjson)
    # library("devtools")
    # library(gtools) # if problems calling library, install.packages("gtools", dependencies = T)
    # library(qdap) # qualitative data analysis package (it masks %>%)
    # library(tm) # framework for text mining; it loads NLP package
    # library(SnowballC);   # wordStem is masked from SnowballC
    # library(stringr)
    # library(dplyr)
    # library(tidyr)
    # library(tidytext)
    # library(ggplot2)
    # library(RColorBrewer)
    # library(wordcloud)
    # library(reshape2)
    # library(RedditExtractoR)
    # library(BBmisc)
    # library(RTextTools)
    # library(e1071)
if(!require("remotes")) install_version("remotes")


shinyServer(
  
  function(input, output) {
    
    ntext <- eventReactive(input$submit, {
 
      
      if(!require("stringi")) install_version("stringi",version='1.1.3')
      querry_edited = gsub(" ","_", "jio")
      
      
      if(!require("RedditExtractoR")) install_version("RedditExtractoR",version='2.0.2')
      reddit_links <- reddit_urls(search_terms= querry_edited,page_threshold = 1)
      
      str(reddit_links)
      
      d=gsub('http:','https:',reddit_links$URL[1])
      reddit_thread <- reddit_content(URL=d)
      str(reddit_thread)
      
      af <-as.data.frame(reddit_thread$comment, stringsAsFactors=FALSE)
      
      colnames(af) = c("data")
      #   af$data <- tolower(af$data)
      
      if(!require("tm")) install_version("tm",version='0.7-1')
      af$data <- tm::removeNumbers(af$data)
      
      if(!require("stringr")) install_version("stringr",version='1.2.0')
      af$data <- str_replace_all(af$data, "  ", "") # replace double spaces with single space #from stringr
      af$data <- str_replace_all(af$data, pattern = "[[:punct:]]", " ")
      
      af$data <- tm::removeWords(x = af$data ,stopwords(kind = "SMART")) #from tm 
      # View(af)
      
      ul <- unlist(strsplit(as.character(af$data), "\\s+|[[:punct:]]"))
      my<-as.data.frame(ul, stringsAsFactors=FALSE)
      my$Index<-seq.int(nrow(my))
      colnames(my) <- c("data","Index")
      
      my <- my[c("Index","data")]
      # View(my)
      
      if(!require("dplyr")) install_version("dplyr",version='0.5.0')
      if(!require("tidytext")) install_version("tidytext",version='0.1.2')
      if(!require("tidyr")) install_version("tidyr",version='0.6.1')
      
      sent <- my %>%
        inner_join(get_sentiments("bing"),by = c("data" = "word")) %>%
        count(data, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
      # View(sent)
      
      bing_word_counts <- my["data"] %>%
        inner_join(get_sentiments("bing"),by = c("data" = "word")) %>%
        count(data,sentiment,sort=TRUE) %>%
        ungroup()
      
      # View(bing_word_counts)
      
      sum(sent$sentiment)
      t=sum(bing_word_counts$n) 
      p=sum(bing_word_counts$n[which(bing_word_counts$sentiment=='positive')])
      n=sum(bing_word_counts$n[which(bing_word_counts$sentiment=='negative')])
      
      p= p/t * 100
      n= n/t*100
      
      if(!require("ggplot2")) install_version("ggplot2",version='2.2.1')
      
      
      output$result <- renderPlot({
        
        bing_word_counts %>%
          group_by(sentiment) %>%
          top_n(10) %>%
          mutate(word = reorder(data, n)) %>%
          ggplot(aes(word, n, fill = sentiment)) +
          geom_bar(stat = "identity", show.legend = FALSE) +
          #  annotate("text", label=p ,x=10,y=10) + 
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = sprintf("Contribution to sentiment \n Positive : %s\n Negative : %s",p,n),x = NULL) +
          coord_flip()
      })
      
      if(!require("shiny")) install_version("shiny",version='1.0.0')
      if(!require("reshape2")) install_version("reshape2",version='1.4.2')
      if(!require("wordcloud")) install_version("wordcloud",version='2.5')
      
      output$result2 <- renderPlot({
        
        my %>%
          inner_join(get_sentiments("bing"), by =c("data" = "word")) %>%
          count(data, sentiment, sort = TRUE) %>%
          acast(data ~ sentiment, value.var = "n", fill = 0) %>%
          comparison.cloud(colors = c("#F8766D", "#00BFC4"),max.words = 100)  
        
      })
      
    })
    
    
    output$text1 <- renderText({ 
      ntext()
      paste("Selected Query : ", input$search)
    })
    
    
    
  
  }
)
