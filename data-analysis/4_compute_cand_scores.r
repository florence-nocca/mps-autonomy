## 4_compute_cand_scores.r transforms the corpuses into dfms and performs different models on them to compute candidates and parties similarity
## It needs to be executed again when a new score is computed to append it to cand_scores.csv

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

## Load corpuses
load("tweets/corpus.Rdata")

## --- First execution only ---
## --- Retrieving candidates social and political characteristics ---
require(readstata13)
data = read.dta13("tweets/french_cand_data.dta")
colnames(data) = tolower(colnames(data))

## Correct wrongly assigned account
index = which(tolower(data$compte) == "lecallennec")
data$compte[index] = "ilecallennec" 
data$compte = tolower(data$compte)

## Keep only candidates mps
data = data[data$represente == 1,]

## Replace starting date by starting year only
data$debutmandat = as.numeric(unlist(regmatches(as.character(data$debutmandat)
, regexec("^[^-]+", as.character(data$debutmandat)
))))

data$naissance = as.numeric(unlist(regmatches(as.character(data$naissance)
, regexec("^[^-]+", as.character(data$naissance)
))))

## Keep only candidates from twCorpus
to_match = data.frame(account = tolower(twCorpus$documents$name))
cand_data = merge(x=to_match, y=data, by.x='account', by.y='compte')
## --- End of first execution ---

## --- Not first execution ---
## Read database on mps, making sure that the parameter na.strings is equal to c("") so that each missing value is coded as a NA
cand_data = read.csv("tweets/cand_scores.csv", header = TRUE, na.strings=c(""))
## --- End ---

## Select parties
## ptwCorpus = corpus_subset(ptwCorpus, name == "partisocialiste" | name == "lesrepublicains" | name == "enmarchefr" | name == "franceinsoumise")
ptwCorpus = corpus_subset(ptwCorpus, name == "partisocialiste" | name == "lesrepublicains" | name == "enmarchefr")

## Add additional stopwords
my_stopwords = as.vector(unlist(read.table("stopwords-fr.txt")))
my_stopwords = c(my_stopwords, "faire", "faut", "veux","veut","oui","non")

## --- Create a document-frequency matrix ---
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

## Stem dfms
ptwdfm = dfm_wordstem(ptwdfm, language = "french")
twdfm = dfm_wordstem(twdfm, language = "french")

## Extract feature labels and document names
head(featnames(twdfm), 30)
head(docnames(ptwdfm))

## 100 most frequent features in the dfm
top_feat = topfeatures(twdfm, n=200)

## Create a wordcloud with most frequent features
par(mfrow=c(1,2), oma = c(0, 0, 10, 0))
textplot_wordcloud(twdfm, max=100)
textplot_wordcloud(ptwdfm, max=100)

## Combine the two corpuses
docvars(twCorpus, "docset") = 1 
docvars(ptwCorpus, "docset") = 2
allCorpus = twCorpus + ptwCorpus

## Transform alldfm to a dfm
alldfm = to_dfm(allCorpus)
alldfm = dfm_wordstem(alldfm, language = "french")
tokenInfo = summary(allCorpus, n = ndoc(allCorpus))

## Barplot of tokens by candidate
barplot(tokenInfo$Tokens, las = 2, ylab = "Nombre de tokens", main = "Nombre de tokens par compte", cex.main = 1.5, xaxt="n")
labs = tolower(tokenInfo$name)
abline(h=mean(tokenInfo$Tokens), col = "blue")
abline(h=median(tokenInfo$Tokens), col = "red")
axis(1, at =(1:length(labs) * 1.2 - 0.5), labels=labs, las=2)
legend('topright',legend = c("Moyenne","Médiane") , lty=1, col=c("blue", "red"), bty='n', cex=.75)
## For a barplot with sorted values: sort(tokenInfo$Tokens, decreasing = TRUE

## Histogram
hist(tokenInfo$Tokens, ylab = "Nombre de candidats", xlab = "Nombre de tokens par candidats", main = "Nombre de tokens par candidats", cex.main = 1.5, col = "gray", xlim = c(min(tokenInfo$Tokens),max(tokenInfo$Tokens)+10000), ylim = c(0,200), breaks = 20)
abline(v=mean(tokenInfo$Tokens), col = "blue")
abline(v=median(tokenInfo$Tokens), col = "red")
legend('topright',legend = c("Moyenne","Médiane") , lty=1, col=c("blue", "red"), bty='n', cex=.75)

## --- Text analysis ---

## --- Wordscores model ---
predictWordscores = function(dfm, virgin_docs, ref_scores)
{
    scores = c(rep(NA, ndoc(virgin_docs)), ref_scores)
    ws = textmodel_wordscores(dfm, scores)
    pred = predict(ws)
    return (pred)
}

## model_1 = predictWordscores(alldfm, twCorpus, ref_scores = c(3.83, 7.67, 5.91, 1.7))
model_1 = predictWordscores(alldfm, twCorpus, ref_scores = c(3.83, 7.67, 5.91))

## Scores to plot
scores = model_1@textscores$textscore_raw

## Differentiate scores from mps and parties
mps_scores = scores[1:ndoc(twCorpus)]
parties_scores = scores[(ndoc(twCorpus)+1):length(scores)]

## Create empty data frame
mps_scores = data.frame(account = twCorpus$documents$name, wordscores = mps_scores, stringsAsFactors = FALSE)

## Remove previously computed wordscores
cand_data = subset(cand_data, select = -wordscores)

## Merge by account names
cand_data = merge(cand_data, mps_scores, by = "account")

hist(cand_data[cand_data$nuance == "SOC",]$wordscores)
hist(cand_data[cand_data$nuance == "LR",]$wordscores)
hist(cand_data[cand_data$nuance == "COM",]$wordscores)
hist(cand_data[cand_data$nuance == "REM",]$wordscores)
hist(cand_data[cand_data$nuance == "UDI",]$wordscores)

write.csv(cand_data, "tweets/cand_scores.csv", row.names = FALSE)
