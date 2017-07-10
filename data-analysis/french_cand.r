rm(list = ls(all = TRUE))

library(quanteda)
library(readtext)
library(topicmodels)
library(tidyr)
library(dplyr)
library(multidplyr)
library(purrr)
library(stringr)

## --- The code below needs only to be executed once ---

## --- Generating a random sample ---
## Create a random sample of 1000 tweets from all tweets
## file = read.csv("tweets/french_cand.tweets.csv", header=TRUE)
## sample = file[sample(1:nrow(file), 1000,
##    replace=FALSE),]
## write.csv(tweets/sample, "sample.csv")

## --- Exploring and subsetting datafile ---
tweets = readtext("tweets/sample.csv", text_field = "text")

## Uncomment to work with full datafile
## tweets = readtext("tweets/french_cand.tweets.csv", text_field = "text")

## Countries in the dataset
unique(tweets$country)
unique(tweets[tweets$country == "Taiwan",]$screen_name)

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
barplot(table(campaign_timeline), las = 2, ylab = "Nombre de tweets", main = "Distribution des tweets durant la campagne", cex.main = 1.5)

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
cand_tweets = readtext("tweets/cand_campaign_tweets.csv", text_field = "text")

## Create a quanteda corpus
twCorpus = corpus(cand_tweets)

## Remove urls
twCorpus$documents$texts = gsub("ht(tps)?[^ ]+","",twCorpus$documents$texts)

## Remove html entities
twCorpus$documents$texts = gsub("&(gt|lt|amp|nbsp);","",twCorpus$documents$texts)

## Optional: remove mentions (@username)
## twCorpus$documents$texts = gsub("@[^ ]+","",twCorpus$documents$texts)

## Remove retweet marker (RT)
twCorpus$documents$texts = gsub("RT","",twCorpus$documents$texts)

## Remove non alphabetical characters and keep accented alpha characters
twCorpus$documents$texts = gsub("[^a-zA-Z\u00C0-\u00FC]", " ", twCorpus$documents$texts)

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

## How many tokens (total words) and features
ntoken(twCorpus)
nfeature(twdfm)

## Arguments to tokenize can be passed 
## ntoken(twCorpus, remove_punct = TRUE)

## How many types (unique words)
ntype(twCorpus)

## Extract feature labels and document names
head(featnames(twdfm), 20)
head(docnames(twdfm))

## Keywords in context
kwic_hamon = kwic(twCorpus$documents$text, "hamon", window = 3)

## --- Create a document-frequency matrix ---

## Add additional stopwords
my_stopwords = as.vector(unlist(read.table("stopwords-fr.txt")))

twdfm = dfm(twCorpus, remove = c(stopwords("french"), "rt", my_stopwords), tolower = TRUE, remove_punct = TRUE, stem = TRUE, remove_twitter = TRUE, remove_numbers = TRUE)

## 100 most frequent features in the dfm
top_feat = topfeatures(twdfm, n=100)
## Create a wordcloud with most frequent features
## textplot_wordcloud(twdfm, max=100)

## --- Diversity, readability and similarity measures ---
## Compute lexical diversity of texts on a dfm
textstat_lexdiv(twdfm, "R")
dotchart(sort(textstat_lexdiv(twdfm, "R")))

## Compute readability of texts on a vector of texts or a corpus:
readab = textstat_readability(cand_tweets$text, 
                               measure = "Flesch.Kincaid")
dotchart(sort(readab))

## Compute document similarities
simil = as.list(textstat_simil(twdfm, margin = "documents", method = "cosine"))
## Only on specified documents 
## as.list(textstat_simil(twdfm, c("cand_tweets.csv.292","cand_tweets.csv.291"), margin = "documents", method = "cosine"))

## --- Hierarchical clustering ---
## Dfm = dfm_trim(twdfm, min_count = 5, min_docfreq = 3)
## Normalize dfm
DistMat = dist(as.matrix(dfm_weight(twdfm, "relFreq")))
## Hierarchical clustering the distance object
Cluster = hclust(DistMat)
## Label with document names
Cluster$labels = docnames(twdfm)
## plot as a dendrogram
plot(Cluster)

## --- Term clustering ---
## Word dendrogram with tf-idf weighting
wordDfm = dfm_sort(dfm_weight(twdfm, "tfidf"))
wordDfm = t(wordDfm)[1:100,]  # because transposed
wordDistMat = dist(wordDfm)
wordCluster = hclust(wordDistMat)
plot(wordCluster, xlab="", main="tf-idf Frequency weighting")

## --- LDA model (topicmodels version) ---
if (require(topicmodels)) {
    myLDAfit = LDA(convert(twdfm, to = "topicmodels"), k = 4)
    get_terms(myLDAfit, 4)
    topics(myLDAfit, 4)
}

## Cluster tweets according to the topics given by the LDA
topic_1 = cand_tweets[topics(myLDAfit) == 1,]$text
topic_2 = cand_tweets[topics(myLDAfit) == 2,]$text
topic_3 = cand_tweets[topics(myLDAfit) == 3,]$text
topic_4 = cand_tweets[topics(myLDAfit) == 4,]$text
## Warning: remember that cleaning was performed on the corpus, not on the cand_tweets datafile

## --- LDA model (quanteda version) ---
## Topic models:
ldadfm = convert(twdfm, to="topicmodels")
lda = LDA(ldadfm, control = list(alpha = 0.1), k = 8)
terms(lda, 10)

## --- k-means model ---
## Number of classes needed
k = 4
kc = kmeans(twdfm, k)

## classes = unlist(lapply(1:k, function(n){
## tweets[kc$cluster == n,]
## }))

## Cluster candidates according to the classes given by the k-means
class_1 = cand_tweets[kc$cluster == 1,]
class_2 = cand_tweets[kc$cluster == 2,]
class_3 = cand_tweets[kc$cluster == 3,]
class_4 = cand_tweets[kc$cluster == 4,]

cand_1 = unique(class_1$name)
cand_2 = unique(class_2$name)
cand_3 = unique(class_3$name)
cand_4 = unique(class_4$name)
