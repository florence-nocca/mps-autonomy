## 1_create_db.r removes wrongly assigned candidates and duplicates
## It needs only to be executed once

rm(list = ls(all = TRUE))

library(quanteda)
library(readtext)
library(tidyr)
library(dplyr)
library(multidplyr)
library(purrr)
library(stringr)

## Use exact values instead of scientific notations
options(scipen=999)

## --- Generating a random sample ---
## Create a random sample of 1000 tweets from all tweets
file = read.csv("tweets/french_cand.tweets.csv", header=TRUE)
sample = file[sample(1:nrow(file), 1000,
   replace=FALSE),]
write.csv(sample, "tweets/sample.csv", row.names = FALSE)

## Comment to work with full datafile
## tweets = readtext("tweets/sample.csv", textfield = "text")

## ## --- Remove wrongly assigned accounts and replace with correct ones ---
tweets = read.csv("tweets/french_cand.tweets.csv", header = TRUE)
ilecallennec = read.csv("tweets/ilecallennec.tweets.csv", header = TRUE)
tweets = tweets[tolower(tweets$screen_name) != "lecallennec",]
tweets = rbind(tweets,ilecallennec)

## Loading parties tweets
ptweets = read.csv("tweets/french_parties.tweets.csv", header = TRUE)

## Removing duplicates
rm_duplicate = function(filename){

    ## Remove line breaks
    filename$text = gsub("\n"," ",filename$text)
    ## Fixing duplicates with different retweet counts that are not detected by unique()
    filename = unique(subset(filename, select = -retweet_count))
}

unique_tweets = rm_duplicate(tweets)
unique_ptweets = rm_duplicate(ptweets)

write.csv(unique_tweets, "tweets/all_french_cand.tweets.csv", row.names = FALSE)
write.csv(unique_ptweets, "tweets/all_french_parties.tweets.csv", row.names = FALSE)
