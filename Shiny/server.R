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
library(twitteR)
# ini gak tau yang kepakek mana aja wkwk
library(e1071)
library(twitteR)
library(ROAuth)
library(ggplot2)
library(sentimentr)
library(devtools)
library(plyr)
library(RTextTools)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # Pemanggilan datanya
  # disesuain tempat datasetnya
  tw = read.csv('D:\\Daigaku\\praktikum2020\\data-raw\\CoronavirusUSA_13_4_to_29_4-2020_Clean_Data3.csv')
  twet_text = tw$tweet_text
  
  # Pembersihan text
  # akan dibuang kata - kata retweet
  twet_text = gsub("b[[:punct:]]","",twet_text)
  # dibuang mention - mention
  twet_text = gsub("@\\w+","",twet_text)
  # dibuang tanda baca
  twet_text = gsub("[[:punct:]]","",twet_text)
  # dibuang angka
  twet_text = gsub("[[:digit:]]","",twet_text)
  # dibuang link - link
  twet_text = gsub("http\\w+","",twet_text)
  # dibuang spasi yang tidak berguna
  twet_text = gsub("[\t]{2,}","",twet_text)
  twet_text = gsub("[\n]","",twet_text)
  twet_text = gsub("^\\s+|\\s+$","",twet_text)
  
  
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
  
  # lower case using try.error with sapply
  some_txt = sapply(twet_text,try.error)
  
  # remove NAs in some_txt
  some_txt = some_txt[!is.na(some_txt)]
  names(some_txt) = NULL
  
  # alamat ini juga disesuaikan
  pos = scan('D:/sentiment/positive-words.txt',what='character', comment.char=';')
  neg = scan('D:/sentiment/negative-words.txt',what='character', comment.char=';')
  
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
  # barplot
  output$distPlot <- renderPlot({
    results = data.frame(tweets = c("Positif", "Negatif", "Netral"), numbers = c(count_positive,count_negative,count_neutral))
    barplot(results$numbers, names = results$tweets, xlab = "Sentiment", ylab = "Counts", col = c("Green","Red","Blue"))
  })
  
  # Output Data analisis
  output$hasil = DT::renderDataTable({
    DT::datatable(hasil, options = list(lengthChange = FALSE))
  })
  
  # Output WordCloud
  output$wordcloud <- renderPlot({ 
    wordcloud(paste(hasil$text, collapse=" "),rot.per=0.25, random.color=TRUE, max.words=100,min.freq = 0 ,colors=brewer.pal(8, "Dark2"))
  }, height=500)
  
  # Output Data analisis
  output$tbl = DT::renderDataTable({
    DT::datatable(tw, options = list(lengthChange = FALSE))
  })
})