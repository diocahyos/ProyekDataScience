#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(wordcloud)
library(tidytext)
library(topicmodels)
library(plyr)
library(dplyr)
library(ggplot2)
library(sentimentr)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # Pemanggilan datanya
  # disesuain tempat datasetnya
  covid <- read.csv("D:/Daigaku/Praktikum/Data Science/ProyekDataScience/data-raw/coronavirususa_clean_data.csv")
  
  # menghapus wkatu pada timesamp sehingga tinggal tanggal saja
  covid <- covid %>% 
    mutate(timestamp = format(as.POSIXct(covid$timestamp,format='%m/%d/%Y%H:%M'),format='%m/%d/%Y'))
  
  # Pembersihan text
  # akan dibuang kata - kata retweet
  twet_text = gsub("b[[:punct:]]","",covid$tweet_text)
  # dibuang mention - mention
  twet_text = gsub("@\\w+","",twet_text)
  # dibuang tanda baca
  twet_text = gsub("[[:punct:]]","",twet_text)
  # dibuang angka
  twet_text = gsub("[[:digit:]]","",twet_text)
  # dibuang link - link
  twet_text = gsub("https\\w+","",twet_text)
  # dibuang spasi yang tidak berguna
  twet_text = gsub("[\t]{2,}","",twet_text)
  twet_text = gsub("[\n]","",twet_text)
  twet_text = gsub("^\\s+|\\s+$","",twet_text)
  # menghapus kata yang depannya x
  twet_text = gsub("x\\w+","",twet_text)
  # dimasukan lagi text yg sudah dibersihkan tadi
  covid = covid %>% mutate(tweet_text = twet_text)
  
  # melakukan transformasi data untuk mencatat kata apa saja yang muncul.
  covid_tidy <- covid %>% 
    filter(!is.na(tweet_text)) %>% 
    mutate(tweet_text = as.character(tweet_text)) %>%
    unnest_tokens(word, tweet_text) %>%
    ungroup() %>%
    anti_join(stop_words)
  
  # Melakukan pemodelan topik. Adapun algoritma yang akan digunakan adalah 
  # Latent Dirichlet allocation (LDA).
  # LDA merupakan algoritma yang biasa digunakan dalam pemodelan topik. 
  # Untuk menjalankan algoritma LDA dari paket `topicmodels`, sebelumnya 
  # harus diubah menjadi obyek berjenis DocumentTermMatrix dengan cara sebagai berikut:
  covid_dtm <- covid_tidy %>% 
    count(timestamp, word) %>% 
    cast_dtm(timestamp, word, n)
  
  # Selanjutnya kita dapat mengimplementasikan algoritma LDA dengan menggunakan 
  # fungsi `LDA()`. Pada fungsi ini harus menentukan nilai k, yaitu jumlah kategori 
  # topik yang diinginkan. disini akan menggunakan nilai k = 5.
  covid_lda <- LDA(covid_dtm, k = 5)
  
  # Mengamati peluang suatu topik per dokumen yang dinyatakan sebagai nilai $gamma$.
  # Fungsi `tidy()` dari paket `broom` dapat digunakan untuk melakukan hal tersebut.
  covid_gamma <- covid_lda %>% 
    tidy(matrix = "gamma") %>% 
    rename(timestamp = document) %>% 
    arrange(timestamp, desc(gamma))
  
  # visualisasi dari hasil diatas
  output$topic <- renderPlot({
    covid_gamma %>% 
      ggplot(aes(x = rev(timestamp), y = gamma, fill = factor(topic))) +
      geom_col() +
      coord_flip() +
      labs(
        x = ",",
        y = expression(gamma),
        title = "Tweet berdasarkan topik",
        fill = "Topik"
      ) +
      theme_light()
  })
  
  # Untuk memahami makna dari setiap topik, dapat menghimpun kata-kata apa saja 
  # yang menjadi kunci dalam suatu topik. Hal tersebut dapat dilakukan dengan cara 
  # mengekstrak probabilitas kata dalam suatu topik yang dinyatakan sebagai nilai $beta$. 
  # disini akan menggunakan fungsi `tidy()` dari paket `broom` untuk 
  # mengekstrak nilai $beta$ dan selanjutnya menampilkan 10 kata teratas dari setiap topik:
  covid_beta <- covid_lda %>% 
    tidy(matrix = "beta") %>% 
    rename(word = term) %>% 
    arrange(topic, desc(beta))
  
  # visualisasi dari hasil diatas
  output$kata <- renderPlot({
    covid_beta %>% 
      filter(topic == input$topic) %>% 
      top_n(10, beta) %>% 
      ggplot(aes(x = reorder(word, beta), y = beta)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "",
        y = expression(gamma),
        title = paste("Kata kunci pada topik",input$topic)
      ) +
      theme_light()
  })
  
  # fungsi error
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try.error = tryCatch(tolower(x),error=function(e)e)
    # if not an error
    if(!inherits(try.error,"error"))
      y = tolower(x)
    # result
    return(y)
  }
  
  # mengambil text pada dataset
  twet_text = covid$tweet_text
  
  # lower case using try.error with sapply
  some_txt = sapply(twet_text,try.error)
  
  # remove NAs in some_txt
  some_txt = some_txt[!is.na(some_txt)]
  names(some_txt) = NULL
  
  # alamat ini juga disesuaikan
  pos = scan('D:/Daigaku/Praktikum/Data Science/ProyekDataScience/positive_words.txt',what='character', comment.char=';')
  neg = scan('D:/Daigaku/Praktikum/Data Science/ProyekDataScience/negative_words.txt',what='character', comment.char=';')
  
  pos.words = c(pos)
  neg.words = c(neg)
  
  # fungsi score sentiment dari text
  score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  { 
    require(plyr)
    require(stringr)
    
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array of scores back, so we use
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)
      
      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words, .progress=.progress )
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  require(corpus)
  require(RCurl)
  
  # dipanggil fungsi tadi dan dimasuki text, positive negative words
  hasil = score.sentiment(some_txt,pos.words,neg.words)
  # View(hasil)
  # sum(hasil$score)
  
  # disini klasifikasi hasil sentimen tadi
  hasil$klasifikasi<- ifelse(hasil$score>0,"Positif", ifelse (hasil$score<0,"Negatif", "Netral"))
  # hasil$klasifikasi
  # View(hasil)
  # str(hasil)
  
  count_positive = 0
  count_negative = 0
  count_neutral = 0
  
  for (i in 1:999){
    
    if (hasil$klasifikasi[i] == "Positif"){
      count_positive = count_positive + 1
    } else if ("Negatif" == hasil$klasifikasi[i]) { 
      count_negative = count_negative + 1
    } else {
      count_neutral = count_neutral + 1
    }
    
  }
  
  # Output WordCloud
  output$wordcloud <- renderPlot({ 
    wordcloud(paste(hasil$text, collapse=" "),rot.per=0.25, random.color=TRUE, max.words=100,min.freq = 0 ,colors=brewer.pal(8, "Dark2"))
  }, height=500)
  
  # barplot
  output$plot <- renderPlot({
    results = data.frame(tweets = c("Positif", "Negatif", "Netral"), numbers = c(count_positive,count_negative,count_neutral))
    barplot(results$numbers, names = results$tweets, xlab = "Sentiment", ylab = "Counts", col = c("Green","Red","Blue"))
  })
  
  # Output Data analisis
  output$hasil = DT::renderDataTable({
    DT::datatable(hasil, options = list(lengthChange = FALSE))
  })
  
})