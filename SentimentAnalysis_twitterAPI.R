library(twitteR)
library(purrr)
library(dplyr)
require('ROAuth')
require('RCurl')
library(plyr)
library(stringr)
#Sentiment Function
#New
score.sentiment<- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores<- laply(sentences, function(sentence, pos.words, neg.words){
    sentence<- gsub('[[:punct:]]',"", sentence)
    sentence<- gsub('[[:cntrl:]]', "", sentence)
    sentence<- gsub('\\d+',"", sentence)
    sentence<- tolower(sentence)
    word.list<- str_split(sentence, '\\s+')
    words<- unlist(word.list)
    pos.matches<- match(sentence, pos.words)
    neg.matches<- match(sentence, neg.words)
    pos.matches<- !is.na(pos.matches)
    neg.matches<- !is.na(neg.matches)
    score<- sum(pos.matches)-sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df<- data.frame(score=scores, text=sentences)
  return(scores.df)
}

##NEW

pos.words = scan('E:/E/DATA SCIENCE/Data Science Projects/20190523 TwitterSentiment/MatchWords/opinion-lexicon-English/positive-words.txt', what = 'character',comment.char = ':')
neg.words = scan('E:/E/DATA SCIENCE/Data Science Projects/20190523 TwitterSentiment/MatchWords/opinion-lexicon-English/negative-words.txt', what = 'character', comment.char = ':')
BJPscore<- score.sentiment(bjptweet_df$text, pos.words, neg.words, .progress='text')
Congscore<- score.sentiment(congtweet_df$text, pos.words, neg.words, .progress='text')
hist(BJPscore$score)
hist(Congscore$score)
consumerKey<- "1DiJ52qtjiPMTVNlPvykrrgkG"
reqURL<- "https://api.twitter.com/oauth/request_token"
accessURL<- "https://api.twitter.com/oauth/access_token"
authURL<- "https://api.twitter.com/oauth/authorize"
consumerSecret<- "blkyCnx06eVyOEQ5DuMOCgWgV1JY8WUG7H619N43DlFshkCoYn"
accessToken<- "1131659181579218956-rwy074hXJNR5XhgOdMj30Cb8Raba8X"
accessTokenSecret<- "kWPtaq3mY6xuhhj2BrFqZ6AbQhwLOPlPAjV0mUzqN3xKx"
twitCred<- OAuthFactory$new(consumerKey=consumerKey,
                            consumerSecret=consumerSecret,
                            requestURL=reqURL,
                            accessURL=accessURL,
                            authURL=authURL)
twitCred$handshake()
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)
tweet1<- userTimeline('@narendramodi', n=200)
tweet2<- userTimeline('@INCIndia', n=200)
bjptweet_df<- tbl_df(map_df(tweet1, as.data.frame))
congtweet_df<- tbl_df(map_df(tweet2, as.data.frame))