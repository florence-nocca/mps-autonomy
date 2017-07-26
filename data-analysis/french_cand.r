## french_cand_r has been subset in several .r The following lines are leftovers that will be sorted into another .r soon.

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

## --- Diversity, readability and similarity measures ---
## Compute lexical diversity of texts on a dfm
textstat_lexdiv(twdfm, "R")
dotchart(sort(textstat_lexdiv(twdfm, "R")))

## Compute readability of texts on a vector of texts or a corpus:
readab = textstat_readability(allCorpus, 
                               measure = "Flesch.Kincaid")
dotchart(sort(readab))
## Compute document similarities
simil = as.matrix(textstat_simil(dfm_weight(twdfm, "relFreq")), margin = "documents", method = "cosine")

simil = as.matrix(textstat_simil(dfm_weight(alldfm, "relFreq"), c("parties_campaign_tweets.csv.1","parties_campaign_tweets.csv.2","parties_campaign_tweets.csv.3","parties_campaign_tweets.csv.4","parties_campaign_tweets.csv.5","parties_campaign_tweets.csv.6","parties_campaign_tweets.csv.7","parties_campaign_tweets.csv.8","parties_campaign_tweets.csv.9")), margin = "documents", method = "cosine")

## Set minimum value to 0
simil = pmax(simil, 0)

## Mean of similarity of each candidate compared to the others
simil_mean = unlist(lapply(1:ndoc(twCorpus),function(cand){
mean(simil[,cand])
}))

## kmeans on similarity matrix
k = 4
km_simil = kmeans(simil, k)

classes = lapply(1:k, function(n){
    unique(cand_tweets[km_simil$cluster == n,]$name)
})


## Create empty data frame
mps_simil = data.frame(account = character(), class_simil = numeric(), stringsAsFactors = FALSE)

## Append data frame with accounts and their corresponding class
for(i in 1:k){
    mps_simil = rbind(mps_simil, data.frame(account = tolower(classes[[i]]), class_simil = rep(i, length(classes[[i]]))))
}

## Merge with db on mps
cand_data = merge(cand_data, mps_simil, by = "account")

## Exploring each class
sum_1 = summary(cand_data[cand_data$class_simil == 1,])
sum_2 = summary(cand_data[cand_data$class_simil == 2,])
sum_3 = summary(cand_data[cand_data$class_simil == 3,])
sum_4 = summary(cand_data[cand_data$class_simil == 4,])
## sum_5 = summary(cand_data[cand_data$class_simil == 5,])

table(cand_data[cand_data$class_simil == 1,]$nuance)
table(cand_data[cand_data$class_simil == 2,]$nuance)
table(cand_data[cand_data$class_simil == 3,]$nuance)
table(cand_data[cand_data$class_simil == 4,]$nuance)
## table(cand_data[cand_data$class_simil == 5,]$nuance)

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
