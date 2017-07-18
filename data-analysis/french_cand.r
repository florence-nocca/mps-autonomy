rm(list = ls(all = TRUE))

library(quanteda)
library(readtext)
library(topicmodels)
library(tidyr)
library(dplyr)
library(multidplyr)
library(purrr)
library(stringr)
library(graphics) # For dendrogram
library(ape) # For dendrogram

## Use exact values instead of scientific notations
options(scipen=999)

## --- The code below needs only to be executed once ---

## ## --- Retrieving candidates social and political characteristics ---
## require(readstata13)
## data = read.dta13("french_cand_data.dta")

## ## Correct wrongly assigned account
## index = which(tolower(data$COMPTE) == "lecallennec")
## data$COMPTE[index] = "ilecallennec" 
## data$COMPTE = tolower(data$COMPTE)

## ## Merge candidates database with mean similarity scores
## to_match = data.frame(account = tolower(twCorpus$documents$name), simil_mean = simil_mean)

## cand_data = merge(x=to_match, y=data, by.x='account', by.y='COMPTE')
## colnames(cand_data) = tolower(colnames(cand_data))

## write.csv(cand_data, "tweets/cand_data.csv")

## --- Generating a random sample ---
## Create a random sample of 1000 tweets from all tweets
## file = read.csv("tweets/french_cand.tweets.csv", header=TRUE)
## sample = file[sample(1:nrow(file), 1000,
##    replace=FALSE),]
## write.csv(sample, "tweets/sample.csv", row.names = FALSE)

## Comment to work with full datafile
## tweets = readtext("tweets/sample.csv", textfield = "text")

## ## --- Remove wrongly assigned accounts and replace with correct ones ---
tweets = read.csv("tweets/french_cand.tweets.csv", header = TRUE)
ilecallennec = read.csv("tweets/ilecallennec.tweets.csv", header = TRUE)
tweets = tweets[tolower(tweets$screen_name) != "lecallennec",]

tweets = rbind(tweets,ilecallennec)

## ## Remove line breaks
tweets$text = gsub("\n"," ",tweets$text)

## Fixing duplicates with different retweet counts that are not detected by unique()
tweets = unique(subset(tweets, select = -retweet_count))

## write.csv(tweets, "tweets/all_french_cand.tweets.csv", row.names = FALSE)

## --- Exploring and subsetting datafile ---
tweets = readtext("tweets/all_french_cand.tweets.csv", textfield = "text")

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
first_tweet = min(grep("2017-03",timeline, value = TRUE))
last_tweet = "2017-06-19"

## Subset the sample to keep campaign tweets only
campaign_tweets = tweets[tweets$created_at >= first_tweet & tweets$created_at <= last_tweet,]
campaign_timeline = as.Date(campaign_tweets$created_at)

## Tweets repartition
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

## Keep only French tweets
tweets = tweets[tweets$lang == "fr",]

## Keep only candidates' names and tweets and create one document per candidate
cand_tweets = data.frame(name = campaign_tweets$screen_name, text = campaign_tweets$text)
cand_tweets = cand_tweets %>% group_by(name) %>% nest() %>% collect()
cand_tweets = cand_tweets %>% group_by(name) %>%
            mutate(data = map(data, function(x) return(paste(as.character(unlist(x)), collapse=" ")))) %>% collect()
cand_tweets$data = unlist(cand_tweets$data)
cand_tweets = data.frame(name = cand_tweets$name, text = cand_tweets$data)

## Write result as csv
write.csv(cand_tweets, "tweets/cand_campaign_tweets.csv")

## --- The code above needs only to be executed once ---

## --- Preparing text for analysis ---
cand_tweets = readtext("tweets/cand_campaign_tweets.csv", textfield = "text")

## Create a quanteda corpus
twCorpus = corpus(cand_tweets)

## Remove urls
twCorpus$documents$texts = gsub("ht(tps)?[^ ]+","",twCorpus$documents$texts)

twCorpus$documents$texts = gsub("(m\\.)?youtube\\.[^ ]*", "", twCorpus$documents$texts, perl=TRUE, ignore.case=TRUE) ## remove Youtube URLs

twCorpus$documents$texts = gsub("(pic\\.)?twitter[^ ]*", "", twCorpus$documents$texts, perl=TRUE, ignore.case=TRUE) ## remove Twitter URLs

twCorpus$documents$texts = gsub("fb.me[^ ]*", "", twCorpus$documents$texts, perl=TRUE, ignore.case=TRUE) ## remove Facebook URLs

twCorpus$documents$texts = gsub("[^ ]*\\.fr", "", twCorpus$documents$texts)

## Remove html entities
twCorpus$documents$texts = gsub("&(gt|lt|amp|nbsp);","",twCorpus$documents$texts)

## Optional: remove mentions (@username)
## twCorpus$documents$texts = gsub("@[^ ]+","",twCorpus$documents$texts)

## Remove retweet marker (RT)
twCorpus$documents$texts = gsub("RT "," ",twCorpus$documents$texts)
## Remove address marker "cc"
twCorpus$documents$texts = gsub(" cc "," ",twCorpus$documents$texts)

## twCorpus$documents$texts = gsub("œ","oe",twCorpus$documents$texts)

## Remove non alphabetical characters (keep accented alpha characters, -, _ and twitter characters)
twCorpus$documents$texts = gsub("[^-_a-zA-Z\u00C0-\u00FC@#œ]", " ", twCorpus$documents$texts)

## Remove numeration marker "er" and "ème"
twCorpus$documents$texts = gsub(" (er)|((è|e)me) "," ",twCorpus$documents$texts)

## Replace abbreviations by entire word
twCorpus$documents$texts = gsub(" ds "," dans ",twCorpus$documents$texts)
twCorpus$documents$texts = gsub(" jms "," jamais ",twCorpus$documents$texts)
twCorpus$documents$texts = gsub(" fr "," france ",twCorpus$documents$texts)
twCorpus$documents$texts = gsub(" pr "," pour ",twCorpus$documents$texts)
twCorpus$documents$texts = gsub(" rf "," république française ",twCorpus$documents$texts)
twCorpus$documents$texts = gsub(" (rdv)(rendez vous) "," rendez-vous ",twCorpus$documents$texts)
twCorpus$documents$texts = gsub(" st( |-)"," saint-",twCorpus$documents$texts)

## Remove one-character words
twCorpus$documents$texts = gsub("^. +"," ",twCorpus$documents$texts) ## at the beginning of the string
twCorpus$documents$texts = gsub(" .$"," ",twCorpus$documents$texts) ## at the end of the string
twCorpus$documents$texts = gsub(" . "," ",twCorpus$documents$texts) ## in the middle of the string

## Remove non-necessary whitespace
twCorpus$documents$texts = gsub(" +", " ", twCorpus$documents$texts) ## in the middle of the string
twCorpus$documents$texts = gsub("^ +| +$", "", twCorpus$documents$texts) ## at both ends of the string

## --- Descriptive statistics on corpus

## Return the number of documents
ndoc(twCorpus)           

## How many tokens (total words)
ntoken(twCorpus)

## Arguments to tokenize can be passed 
## ntoken(twCorpus, remove_punct = TRUE)

## How many types (unique words)
ntype(tolower(twCorpus))

## carpentierjn: only one tweet
## Candidates that are to be removed
## removed = twCorpus$documents$name[ntype(tolower(twCorpus)) < 50]

## Keep only documents with more than 40 types
twCorpus = corpus_subset(twCorpus, ntype(tolower(twCorpus)) > 50)

## Keywords in context
kwic_hamon = kwic(tolower(twCorpus$documents$text), "hamon", window = 3)
kwic_marché = kwic(tolower(twCorpus$documents$text), "marché", window = 3)

## --- Create a document-frequency matrix ---

## Add additional stopwords
my_stopwords = as.vector(unlist(read.table("stopwords-fr.txt")))
my_stopwords = c(my_stopwords, "faire", "faut", "veux","veut","oui","non")

twdfm = dfm(twCorpus, remove = c(stopwords("french"), stopwords("english"), my_stopwords), tolower = TRUE, remove_punct = TRUE, stem = FALSE, remove_twitter = TRUE, remove_numbers = TRUE)

## Extract feature labels and document names
head(featnames(twdfm), 30)
head(docnames(twdfm))

## 100 most frequent features in the dfm
top_feat = topfeatures(twdfm, n=200)
## Create a wordcloud with most frequent features
textplot_wordcloud(twdfm, max=100)

## --- Diversity, readability and similarity measures ---
## Compute lexical diversity of texts on a dfm
textstat_lexdiv(twdfm, "R")
dotchart(sort(textstat_lexdiv(twdfm, "R")))

## Compute readability of texts on a vector of texts or a corpus:
readab = textstat_readability(cand_tweets$text, 
                               measure = "Flesch.Kincaid")
dotchart(sort(readab))
## Compute document similarities
simil = as.matrix(textstat_simil(dfm_weight(twdfm, "relFreq")), margin = "documents", method = "cosine")

## Set minimum value to 0
simil = pmax(simil, 0)

## Mean of similarity of each candidate compared to the others
simil_mean = unlist(lapply(1:ndoc(twCorpus),function(cand){
mean(simil[,cand])
}))

## kmeans on similarity matrix
km_simil = kmeans(simil, 5)

## Only on specified documents 
## as.list(textstat_simil(twdfm, c("cand_tweets.csv.292","cand_tweets.csv.291"), margin = "documents", method = "cosine"))

## --- Hierarchical clustering ---
## Dfm = dfm_trim(twdfm, min_count = 5, min_docfreq = 3)
## Normalize dfm
DistMat = dist(as.matrix(dfm_weight(twdfm, "relFreq")))
## Hierarchical clustering the distance object
Cluster = hclust(DistMat)
## Label with document names
Cluster$labels = tolower(twCorpus$documents$name)
## plot as a dendrogram
plot(as.dendrogram(Cluster), horiz = FALSE)

pdf(file="Dendrogram.pdf")
plot(as.phylo(Cluster), type = "fan", cex = 0.5, no.margin=TRUE)
dev.off()

classes = cutree(Cluster, k=4)

classes = lapply(1:k, function(n){
    cand_tweets[classes == n,]$name
})


## --- Term clustering ---
## Word dendrogram with tf-idf weighting
wordDfm = dfm_sort(dfm_weight(twdfm, "tfidf"))
wordDfm = t(wordDfm)[1:100,]  # because transposed
wordDistMat = dist(wordDfm)
wordCluster = hclust(wordDistMat)
plot(wordCluster, xlab="", main="tf-idf Frequency weighting")

## --- LDA model (topicmodels version) ---
k = 4
if (require(topicmodels)) {
    myLDAfit = LDA(convert(twdfm, to = "topicmodels"), k = k)
    get_terms(myLDAfit, 4)
    topics(myLDAfit, 4)
}

## Cluster tweets according to the topics given by the LDA
topics = lapply(1:k, function(n){
    cand_tweets[topics(myLDAfit) == n,]$text
})

## Warning: remember that cleaning was performed on the corpus, not on the cand_tweets datafile

## --- LDA model (quanteda version) ---
## Topic models:
ldadfm = convert(twdfm, to="topicmodels")
lda = LDA(ldadfm, control = list(alpha = 0.1), k = 8)
terms(lda, 10)

## --- k-means model ---
## Number of classes needed
k = 5
kc = kmeans(twdfm, k)

## Cluster candidates according to the classes given by the k-means
classes = lapply(1:k, function(n){
    unique(cand_tweets[kc$cluster == n,]$name)
})
