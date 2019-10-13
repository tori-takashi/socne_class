install.packages("rtweet")
install.packages("ggplot2")
install.packages("stringr")
install.packages("plyr")
install.packages("RMeCab", repos = "http://rmecab.jp/R")

library(rtweet)
library(ggplot2)
library(stringr)
library(plyr)
library(RMeCab)

twitter_token <- create_token(
  app = "SOCNE_class",
  consumer_key = NULL,   #should type yours
  consumer_secret = NULL #should type youre
)

greta_jp <- search_tweets(
  "グレタ",
  n=10000,
  include_rts=FALSE
)

greta_en <- search_tweets(
  "greta",
  n=10000,
  include_rts=FALSE
)

# Credit for score_sentiment(fork version): Adrian Rauchfleisch
score_sentiment <- function(sentences, pos.words, neg.words, .progress='none', japanese=FALSE) {
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  
  scores <- laply(sentences,
                  function(sentence, pos.words, neg.words)
                  {
                    if ( !japanese ) {
                      # remove punctuation
                      sentence <- gsub("[[:punct:]]", "", sentence)
                      # remove control characters
                      sentence <- gsub("[[:cntrl:]]", "", sentence)
                      # remove digits?
                      sentence <- gsub('\\d+', '', sentence)
                      
                      # define error handling function when trying tolower
                      tryTolower <- function(x)
                      {
                        # create missing value
                        y <- NA
                        # tryCatch error
                        try_error <- tryCatch(tolower(x), error=function(e) e)
                        # if not an error
                        if (!inherits(try_error, "error"))
                          y <- tolower(x)
                        # result
                        return(y)
                      }
                      # use tryTolower with sapply 
                      # if japanese, don't use tolower:
                      sentence <- sapply(sentence, tryTolower)
                    }
                    
                    if ( !japanese) {
                      # split sentence into words with str_split (stringr package)
                      word.list <- str_split(sentence, "\\s+")
                      words <- unlist(word.list)
                    } else {
                      word.list <- RMeCabC(enc2utf8(sentence))
                      words <- unlist(word.list) 
                    }
                    
                    # compare words to the dictionaries of positive & negative terms
                    pos.matches <- match(words, pos.words)
                    neg.matches <- match(words, neg.words)
                    
                    # get the position of the matched term or NA
                    # we just want a TRUE/FALSE
                    pos.matches <- !is.na(pos.matches)
                    neg.matches <- !is.na(neg.matches)
                    
                    # final score
                    score <- sum(pos.matches) - sum(neg.matches)
                    return(score)
                  }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df <- data.frame(text=sentences, score=scores)
  return(scores.df)
}

pos_jp <- readLines("japanese_positive.dict")
neg_jp <- readLines("japanese_negative.dict")

pos_en <- readLines("english_positive.dict")
neg_en <- readLines("english_negative.dict")

greta_jp <- greta_jp[greta_jp$source != "twittbot.net",]

greta_jp_text <- greta_jp$text
greta_en_text <- greta_en$text

greta_jp_text <- str_replace_all(greta_jp_text,"[^[:graph:]]", " ")
greta_en_text <- str_replace_all(greta_en_text,"[^[:graph:]]", " ")

greta_text <- c(greta_jp_text, greta_en_text)

score_jp <- score_sentiment(greta_jp_text,pos_jp, neg_jp, .progress="text", japanese=TRUE  )
score_en <- score_sentiment(greta_en_text,pos_en, neg_en, .progress="text", japanese=FALSE )

mean(score_en$score)
mean(score_jp$score)

ggplot(score_jp, aes(x="Japanese", y=score, group="Japanese")) +
  geom_boxplot(aes(fill="Japanese")) +
  scale_fill_manual(values="#7CAE00") +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) +
  ggtitle(title <- "Boxplot Japanese")

ggplot(score_en, aes(x="English", y=score, group="English")) +
  geom_boxplot(aes(fill="English")) +
  scale_fill_manual(values="#00BFC4") +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) +
  ggtitle(title <- "Boxplot English")