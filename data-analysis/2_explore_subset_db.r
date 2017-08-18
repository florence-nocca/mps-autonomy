## 2_explore_subset_db.r subsets the databases according to language, campaign dates and paste in one variable all the tweets written by a given candidate
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

tweets = readtext("data/all_french_cand.tweets.csv", textfield = "text")
ptweets = readtext("data/all_french_parties.tweets.csv", textfield = "text")

## Countries in the dataset
unique(tweets$country)
unique(tweets[tweets$country == "Taiwan",]$screen_name)

## Language repartition in the dataset
barplot(table(tweets$lang))
## Top languages: fr, en, sp (und: undetermined, tweets containing urls or mentions only)
## How many tweets not written in French
length(unique(tweets[tweets$lang != "fr",]$text))

## Timeline
timeline = as.Date(unique(tweets$created_at))
boxplot(timeline)

## Define upper and lower bounds
first_tweet = as.Date("2017-03-01")
last_tweet = as.Date("2017-06-18")
## last_tweet = as.Date("2017-06-11")

## Subset the sample to keep campaign tweets only
campaign_subset = function(filename){

    campaign_filename = filename[as.Date(filename$created_at) >= first_tweet & as.Date(filename$created_at) <= last_tweet,]

}

campaign_tweets = campaign_subset(tweets)
campaign_ptweets = campaign_subset(ptweets)

## Tweets repartition
campaign_timeline = as.Date(campaign_tweets$created_at)
### By weekday
barplot(table(sort(weekdays(campaign_timeline))))
## By month
barplot(table(months(campaign_timeline)))

## During the whole period
## Create a vector containing all days included in the campaign
campaign_days = seq(as.Date("2017/3/1"), as.Date("2017/6/18"), "days")

## Create a table of number of tweets per day and remove one per cell (as campaign_days artificially adds one tweet per cell)
tw_per_days = table(c(campaign_days,campaign_timeline)) - 1

barplot(tw_per_days, las = 2, ylab = "Nombre de tweets", main = "Distribution des tweets durant la campagne", cex.main = 1.5, xaxt="n")
labs = substr((campaign_days),6,10)
axis(1, at =(1:length(labs) * 1.2 - 0.5), labels=labs, las=2)

## Explore peaks
start_date = "2017-04-04 00:00:00"
end_date = "2017-04-05 00:00:00"

sub_tweets = tweets[tweets$created_at >= start_date & tweets$created_at <= end_date,]
sample(sub_tweets$text,15)

## ## Counting tweets per candidates (before June 11th 2017 only)
## filename = campaign_tweets
## filename = data.frame(name = tolower(filename$screen_name), text = filename$text)
## filename = filename %>% group_by(name) %>% collect()
## table_tweets = table(filename$name)
## nb_tweets_mp = data.frame(account = names(table_tweets), nb_tweets = as.numeric(table_tweets))

## cand_data = read.csv("data/cand_scores.csv", header = TRUE, na.strings=c(""))
## cand_data = merge(cand_data, nb_tweets_mp, by = "account")
## Write changes to cand_scores file
## write.csv(cand_data, "data/cand_scores.csv", row.names = FALSE)

## Creating one-author documents
sort_by_author = function(filename)
{
    ## Keep only French filename
    filename = filename[filename$lang == "fr",]
    
    ## Keep only candidates' names and filename and create one document per candidate
    filename = data.frame(name = filename$screen_name, text = filename$text)
    filename = filename %>% group_by(name) %>% nest() %>% collect()
    filename = filename %>% group_by(name) %>%
        mutate(data = map(data, function(x) return(paste(as.character(unlist(x)), collapse=" ")))) %>% collect()
    filename$data = unlist(filename$data)
    filename = data.frame(name = filename$name, text = filename$data)
}

cand_tweets = sort_by_author(campaign_tweets)
parties_tweets = sort_by_author(campaign_ptweets)

## Write result as csv
write.csv(cand_tweets, "data/cand_campaign_tweets.csv")
write.csv(parties_tweets, "data/parties_campaign_tweets.csv")
