# ini gak tau yang kepakek mana aja wkwk
library(e1071)
library(twitteR)
library(ROAuth)
library(tm)
library(ggplot2)
library(wordcloud)
library(sentimentr)
library(devtools)
library(plyr)
library(RTextTools)

# Pemanggilan datanya
# disesuain tempat datasetnya
tw = read.csv('D:\\Daigaku\\praktikum2020\\data-raw\\CoronavirusUSA_13_4_to_29_4-2020_Clean_Data3.csv')
twet_text = tw$tweet_text
twet_text
str(tw)

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

twet_text

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
some_txt

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

# dipanggil fungsi tadi dan dimasuki text, positive negative words
hasil = score.sentiment(some_txt,pos.words,neg.words)
View(hasil)
sum(hasil$score)

# disini klasifikasi hasil sentimen tadi
hasil$klasifikasi<- ifelse(hasil$score>0,"Positif", ifelse (hasil$score<0,"Negatif", "Netral"))
hasil$klasifikasi
View(hasil)

data <- hasil[c(3,1,2)]
View(data)
write.csv(data, file = "Label_Tweet_covid.csv")
text<-hasil[2]
View(text)
write.table(text, file = 'tweet_covid.txt', sep = '')

