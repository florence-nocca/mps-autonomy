## 6_network_analysis.r extract mentions from tweets

rm(list = ls(all = TRUE))

library(quanteda)
library(stringr)
library(readtext)

## Use exact values instead of scientific notations
options(scipen=999)

## First run to create network corpuses
## ## Load corpuses
## load("data/corpus.Rdata")

## ## Keep only @mentions in tweets
## extractMentions = function(text)
## {
##     text = paste(str_extract_all(text,"@[^ ]+", simplify = TRUE))
##     text = gsub("@","",text)
##     return(text)
## }

## pmentions = lapply(ptwCorpus$documents$text, function(text)
## {
##     text = extractMentions(text)
##     text = paste(text,collapse=" ")
## })

## ptwCorpus$documents$texts = tolower(pmentions)

## docnames(ptwCorpus) = paste(docvars(ptwCorpus, "name"))

## ptwCorpus$documents$party = c("SOC","LR","PG","PCF","FN","REM","ECO","UDI","FI")


## mentions = lapply(twCorpus$documents$text, function(text)
## {
##     text = extractMentions(text)
##     text = paste(text,collapse=" ")
## })

## twCorpus$documents$texts = tolower(mentions)
## ## Remove documents without any mentions
## twCorpus = corpus_subset(twCorpus, ntype(tolower(twCorpus)) > 0)

## ## Add party as twCorpus' docvar
## cand_data = read.csv("data/cand_scores.csv", header = TRUE, na.strings=c(""))
## cand_party = data.frame(party = cand_data$nuance, row.names = cand_data$account) 

## twCorpus = corpus_subset(twCorpus, name %in% as.character(cand_data$account))

## twCorpus$documents$party = as.character(cand_party[as.character(twCorpus$documents$name),])

## ## Indicate name and party as docname
## docnames(twCorpus) = paste(docvars(twCorpus, "name"), docvars(twCorpus, "party"))

## ## Save corpuses as R objects
## save(twCorpus, ptwCorpus, file = "data/net_corpus.Rdata")


## Load corpuses
load("data/net_corpus.Rdata")
