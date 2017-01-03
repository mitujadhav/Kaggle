
library("twitteR")
library("ROAuth")
library(base64enc)
library(httr)
library(rjson)
library(tm)
set_config(config(ssl_verifypeer = 0L))
devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")


library(devtools)
install_github('twitteR', 'geoffjentry')
library (twitteR)

customer_key<-"0nYLljhOBUpOYyN6aefRoDfGH"
consumer_secret<-"WHnaSWMPxP6iTQJjOTXfudbnWOc2HA22I7Qh8e0fuvx1uDKWFX"
access_token<-"703023948-Jo2U2qACW6LPeYbNGHqB35qKEHJKCymfeOSJDvxz"
access_secret<-"MtndyGAiNGx2Q4ssTI4faJeHR8rrAIltUNeoVljUuOmLS"

setup_twitter_oauth(customer_key, consumer_secret, access_token, access_secret)


#mySentiment <- get_nrc_sentiment(narendra_tweets)
narendra_tweets = searchTwitter("#Blackmoney")


writeLines(toJSON(narendra_tweets), "afile.JSON")
