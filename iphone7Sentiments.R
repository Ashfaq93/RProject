library(tm)
library(plyr)
library(stringr)
library(wordcloud)
library(twitteR)
library(Rstem)
library(tidytext)
Tweets_apple <- read.csv("C:/Users/Ashfaq/Downloads/Tweets_apple.csv", comment.char="#", stringsAsFactors=FALSE)

Tweets_apple$X <- NULL
names(Tweets_apple)
str(Tweets_apple)

txtFromTweets = VectorSource(Tweets_apple$text)
corpus <- VectorSource(Tweets_apple$text)
catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}
corpus <- sapply(corpus,catch.error)
##cleaning using tm package
corpus <- tm_map(corpus, content_transformer(tolower))
# the following line may or may not be needed, depending on
# your tm  package version
# corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)


##cleaning the text using regular expressions

txtFromTweets = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",txtFromTweets) ##
txtFromTweets = gsub("http[^[:blank:]]+", "", txtFromTweets)## removing the patterns starts with https
txtFromTweets = gsub("@\\w+", "", txtFromTweets)##
txtFromTweets = gsub("[ \t]{2,}", "", txtFromTweets)##
txtFromTweets = gsub("^\\s+|\\s+$", "", txtFromTweets)## removing the white space (leading and trailing)
txtFromTweets = gsub("[[:punct:]]", " ", txtFromTweets) ## removing the punctuations
txtFromTweets = gsub("[^[:alnum:]]", " ", txtFromTweets) ## removing the alpha numeric values
txtFromTweets <- gsub('\\d+', '', txtFromTweets) ## removing the numeric values

dmatrix <- DocumentTermMatrix(txtFromTweets)

###sentiment analysis

pos <- scan("D:\\Users\\Ashfaq\\Downloads\\gmu 2nd sem\\SYST 542\\project data\\positive-words.txt", skip = 35, what = "character")
neg <- scan("D:\\Users\\Ashfaq\\Downloads\\gmu 2nd sem\\SYST 542\\project data\\negative-words.txt", skip = 35, what = "character")

# add industry-specific terms...
pos_words <- c(pos, "happy")
neg_words <- c(neg, "dammit", "recall", "safety", "crash", "accident", "malfunction")

# sentiment_score function
sentiment_score <- function(txtFromTweets, pos_words, neg_words) {
  library(stringr)
  # split text into words
  ##word_list <- str_split(txtFromTweets, '\\s+')
  # unlist
##  words <- unlist(word_list)
  
  # compare rods to the dictionaries of positive and negative terms
  # return only those that are TRUE/FALSE
#  pos_matches <- !is.na(match(words, pos_words))
#  neg_matches <- !is.na(match(words, neg_words))
  
  # score. TRUE/FAlSE treated as 1/0 by sum()
 # score <- sum(pos_matches) - sum(neg_matches)
  #return(score)
  
  scores = lapply(txtFromTweets, function(sentence, pos_words, neg_words) {
    
    
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos_words)
    neg.matches = match(words, neg_words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos_words, neg_words)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

library(e1071)

