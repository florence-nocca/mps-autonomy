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

## Take only hours
time_tweeted = sub(".* ", "", tweets$created_at)
hours_tweeted = as.integer(sub(":.*", "", time_tweeted))
## Tweets are mostly published between 5am and 8pm, the majority between 3pm and 8pm

## Tweets' sources
sources = table(tweets$source)

## By candidate
source_by_cand = data.frame(name = tolower(tweets$screen_name), source = tweets$source)
source_by_cand = source_by_cand %>% group_by(name) %>% collect()
source_by_cand$source = as.character(source_by_cand$source)

## Sources to be removed or changed
source_by_cand$source = gsub("JH Wordpress Autopost","WordPress.com",source_by_cand$source)
source_by_cand$source = gsub("®","",source_by_cand$source)
source_by_cand$source = gsub(" Kiwi","",source_by_cand$source)
source_by_cand$source = gsub("#Fillon2017",NA,source_by_cand$source)
source_by_cand$source = gsub("Hamon2017Mobile",NA,source_by_cand$source)
source_by_cand$source = gsub("FollowTweetFavorite",NA,source_by_cand$source)
source_by_cand$source = gsub("Paris 2024",NA,source_by_cand$source)
source_by_cand$source = gsub("ILC35",NA,source_by_cand$source)
source_by_cand$source = gsub(" "," ",source_by_cand$source)

un_source_by_cand = unique(source_by_cand)
nb_source = sort(table(un_source_by_cand$name), decreasing = T)
nb_source = na.omit(nb_source)

hist(nb_source)


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
## By month
barplot(table(months(campaign_timeline)))

## By weekday
barplot(table(sort(weekdays(campaign_timeline))))
## Too sensitive to number of days (4 weekends where campaigning was prohibited) 
### Create a vector containing all days included in the campaign
campaign_days = seq(as.Date("2017/3/1"), as.Date("2017/6/18"), "days")

days_to_remove = c("2017-04-22","2017-04-23","2017-05-06","2017-05-07","2017-06-10","2017-06-11","2017-06-17","2017-06-18")

short_campaign_timeline = campaign_timeline[-(which(campaign_timeline %in% as.Date(days_to_remove)))]

## Count weekdays occurences
days = table(weekdays(campaign_days))
days["Saturday"] = days["Saturday"] - 4
days["Sunday"] = days["Sunday"] - 4

weighted_days = sort(table(sort(weekdays(as.Date(short_campaign_timeline)))) / days)

mean(weighted_days)
sd(weighted_days)

## During the whole period
## Create a table of number of tweets per day and remove one per cell (as campaign_days artificially adds one tweet per cell)
tw_per_days = table(c(campaign_days,campaign_timeline)) - 1

pdf("Graphs/Barplot_tw_per_days.pdf")
barplot(tw_per_days, las = 2, ylab = "Nombre de tweets", main = "Distribution des tweets durant la campagne", cex.main = 1.5, xaxt="n")
labs = substr((campaign_days),6,10)
axis(1, at =(1:length(labs) * 1.2 - 0.5), labels=labs, las=2)
dev.off()

## Explore peaks
start_date = "2017-06-08 00:00:00"
end_date = "2017-06-10 00:00:00"

sub_tweets = tweets[tweets$created_at >= start_date & tweets$created_at <= end_date,]
sample(sub_tweets$text,15)

## Counting tweets per candidates (nb_tweets -> before June 11th 2017 only and nb_tweets_all -> whole period)
## filename = campaign_tweets
## filename = data.frame(name = tolower(filename$screen_name), text = filename$text)
## filename = filename %>% group_by(name) %>% collect()
## table_tweets = table(filename$name)
## nb_tweets_mp = data.frame(account = names(table_tweets), nb_tweets_all = as.numeric(table_tweets))

## cand_data = read.csv("data/cand_scores.csv", header = TRUE, na.strings=c(""))
## cand_data = merge(cand_data, nb_tweets_mp, by = "account")
## ## Write changes to cand_scores file
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
