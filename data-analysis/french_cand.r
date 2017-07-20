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
## tweets = read.csv("tweets/french_cand.tweets.csv", header = TRUE)
## ilecallennec = read.csv("tweets/ilecallennec.tweets.csv", header = TRUE)
## tweets = tweets[tolower(tweets$screen_name) != "lecallennec",]
## tweets = rbind(tweets,ilecallennec)

## ## Loading parties tweets
## ptweets = read.csv("tweets/french_parties.tweets.csv", header = TRUE)

## ## Removing duplicates
## rm_duplicate = function(filename){

##     ## Remove line breaks
##     filename$text = gsub("\n"," ",filename$text)
##     ## Fixing duplicates with different retweet counts that are not detected by unique()
##     filename = unique(subset(filename, select = -retweet_count))
## }

## unique_tweets = rm_duplicate(tweets)
## unique_ptweets = rm_duplicate(ptweets)

## write.csv(unique_tweets, "tweets/all_french_cand.tweets.csv", row.names = FALSE)
## write.csv(unique_ptweets, "tweets/all_french_parties.tweets.csv", row.names = FALSE)

## --- Exploring and subsetting datafile ---
tweets = readtext("tweets/all_french_cand.tweets.csv", textfield = "text")

ptweets = readtext("tweets/all_french_parties.tweets.csv", textfield = "text")

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
write.csv(cand_tweets, "tweets/cand_campaign_tweets.csv")
write.csv(parties_tweets, "tweets/parties_campaign_tweets.csv")

## --- The code above needs only to be executed once ---

## --- Preparing text for analysis ---
cand_tweets = readtext("tweets/cand_campaign_tweets.csv", textfield = "text")

parties_tweets = readtext("tweets/parties_campaign_tweets.csv", textfield = "text")

## Create a quanteda corpus
twCorpus = corpus(cand_tweets)
ptwCorpus = corpus(parties_tweets)

## Clean corpuses
clean = function(corpus_texts)
{

    corpus_texts = gsub("ht(tps)?[^ ]+","",corpus_texts)

    corpus_texts = gsub("(m\\.)?youtube\\.[^ ]*", "", corpus_texts, perl=TRUE, ignore.case=TRUE) ## remove Youtube URLs

    corpus_texts = gsub("(pic\\.)?twitter[^ ]*", "", corpus_texts, perl=TRUE, ignore.case=TRUE) ## remove Twitter URLs

    corpus_texts = gsub("fb.me[^ ]*", "", corpus_texts, perl=TRUE, ignore.case=TRUE) ## remove Facebook URLs

    corpus_texts = gsub("[^ ]*\\.fr", "", corpus_texts)

    ## Remove html entities
    corpus_texts = gsub("&(gt|lt|amp|nbsp);","",corpus_texts)

    corpus_texts = gsub("\n"," ",corpus_texts)

    ## Optional: remove mentions (@username)
    ## corpus_texts = gsub("@[^ ]+","",corpus_texts)

    ## Remove retweet marker (RT)
    corpus_texts = gsub("RT "," ",corpus_texts)
    ## Remove address marker "cc"
    corpus_texts = gsub(" cc "," ",corpus_texts)

    ## corpus_texts = gsub("œ","oe",corpus_texts)

    ## Remove non alphabetical characters (keep accented alpha characters, -, _ and twitter characters)
    corpus_texts = gsub("[^-_a-zA-Z\u00C0-\u00FC@#œ]", " ", corpus_texts)

    ## Remove numeration marker "er" and "ème"
    corpus_texts = gsub(" (er)|((è|e)me) "," ",corpus_texts)

    ## Replace abbreviations by entire word
    corpus_texts = gsub(" ds "," dans ",corpus_texts)
    corpus_texts = gsub(" dt "," dont ",corpus_texts)
    corpus_texts = gsub(" jms "," jamais ",corpus_texts)
    corpus_texts = gsub(" fr "," france ",corpus_texts)
    corpus_texts = gsub(" pr "," pour ",corpus_texts)
    corpus_texts = gsub(" rf "," république française ",corpus_texts)
    corpus_texts = gsub(" (rdv)(rendez vous) "," rendez-vous ",corpus_texts)
    corpus_texts = gsub(" st( |-)"," saint-",corpus_texts)

    ## Remove one-character words
    corpus_texts = gsub("^. +"," ",corpus_texts) ## at the beginning of the string
    corpus_texts = gsub(" .$"," ",corpus_texts) ## at the end of the string
    corpus_texts = gsub(" . "," ",corpus_texts) ## in the middle of the string

    ## Remove non-necessary whitespace
    corpus_texts = gsub(" +", " ", corpus_texts) ## in the middle of the string
    corpus_texts = gsub("^ +| +$", "", corpus_texts) ## at both ends of the string
}

twCorpus$documents$texts = clean(twCorpus$documents$texts)
ptwCorpus$documents$texts = clean(ptwCorpus$documents$texts)

## --- Descriptive statistics on corpus
## Return the number of documents
ndoc(twCorpus)           

## How many tokens (total words)
ntoken(twCorpus)

## Arguments to tokenize can be passed 
## ntoken(twCorpus, remove_punct = TRUE)

## How many types (unique words)
ntype(tolower(twCorpus))

## Candidates that are to be removed
## removed = twCorpus$documents$name[ntype(tolower(twCorpus)) < 50]

## Keep only documents with more than 40 types
twCorpus = corpus_subset(twCorpus, ntype(tolower(twCorpus)) > 50)

## Keywords in context
kwic_hamon = kwic(tolower(twCorpus$documents$text), "hamon", window = 3)
kwic_marché = rbind(kwic(tolower(twCorpus$documents$text), "marché", window = 3),kwic(tolower(ptwCorpus$documents$text), "marché", window = 3))

## --- Create a document-frequency matrix ---

## Add additional stopwords
my_stopwords = as.vector(unlist(read.table("stopwords-fr.txt")))
my_stopwords = c(my_stopwords, "faire", "faut", "veux","veut","oui","non")

to_dfm = function(corpus, groups = NULL)
{
    twdfm = dfm(corpus,
                remove = c(stopwords("french"), stopwords("english"), my_stopwords),
                tolower = TRUE,
                remove_punct = TRUE,
                stem = FALSE,
                remove_twitter = TRUE,
                remove_numbers = TRUE,
                groups = groups)
}



twdfm = to_dfm(twCorpus)
ptwdfm = to_dfm(ptwCorpus)

## Extract feature labels and document names
head(featnames(ptwdfm), 30)
head(docnames(ptwdfm))

## 100 most frequent features in the dfm
top_feat = topfeatures(twdfm, n=200)

## Create a wordcloud with most frequent features
par(mfrow=c(1,2), oma = c(0, 0, 10, 0))
textplot_wordcloud(twdfm, max=100)
textplot_wordcloud(ptwdfm, max=100)

docvars(twCorpus, "docset") = 1 
docvars(ptwCorpus, "docset") = 2
allCorpus = twCorpus + ptwCorpus
summary(allCorpus, 5)

alldfm = to_dfm(allCorpus,"docset")

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


## Only on specified documents 
## as.list(textstat_simil(twdfm, c("cand_tweets.csv.292","cand_tweets.csv.291"), margin = "documents", method = "cosine"))

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

## Merge with database on mps, making sure that the parameter na.strings is equal to c("") so that each missing value is coded as a NA
cand_data = read.csv("tweets/cand_data.csv", header = TRUE, na.strings=c(""))

## Keep only candidates mps
cand_data = cand_data[cand_data$represente == 1,]

## Replace starting date by starting year only
cand_data$debutmandat = as.numeric(unlist(regmatches(as.character(cand_data$debutmandat)
, regexec("^[^-]+", as.character(cand_data$debutmandat)
))))

cand_data$naissance = as.numeric(unlist(regmatches(as.character(cand_data$naissance)
, regexec("^[^-]+", as.character(cand_data$naissance)
))))

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


library(knitr)
library(caret)
library(gmodels)
library(lattice)
library(ggplot2)
library(gridExtra)
library(Kmisc)
library(ROCR)
library(corrplot)

## --- Exploratory Analysis ---
set.seed(1023)

## Looking at data structure
str(cand_data)

cand_data_subset = subset(cand_data, select = c(account,sex,naissance,nuance,class_simil))
colnames(cand_data_subset)

cand_data_subset$class_simil = as.factor(cand_data_subset$class_simil)

cand_data_subset$naissance = as.factor(cand_data_subset$naissance)

cand_data_subset$sex = as.factor(cand_data_subset$sex)

kable(head(cand_data_subset))

## Counting NAs
cols_withNa = apply(cand_data_subset, 2, function(x) sum(is.na(x)))
## No NA in the dataset

## --- Categorical Variable Analysis ---
factor_vars = names(which(sapply(cand_data_subset, class) == "factor"))
factor_vars = setdiff(factor_vars, "class_simil")
factor_vars = setdiff(factor_vars, "account")

chisq_test_res = lapply(factor_vars, function(x) { 
    chisq.test(cand_data_subset[,x], cand_data_subset[, "class_simil"], simulate.p.value = TRUE)
})

names(chisq_test_res) = factor_vars

## Chi-squared p-value results tell us that class simil values do not depend upon nuance nor naissance (as p.value > 0.05)
chisq_test_res

## Significativeness of given categorical variables with barcharts with the cross table row proportions as input
barchart_res = lapply(factor_vars, function(x) { 
    title = colnames(cand_data_subset[,x, drop=FALSE])
    wgd = CrossTable(cand_data_subset[,x], cand_data_subset$class_simil, prop.chisq=F)
    barchart(wgd$prop.row, stack=F, auto.key=list(rectangles = TRUE, space = "top", title = title))
})

names(barchart_res) = factor_vars
barchart_res$nuance
barchart_res$naissance
barchart_res$sex

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
