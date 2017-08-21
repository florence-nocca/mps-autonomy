## 3_clean_db_to_corpus.r cleans the tweets csv, transforms them into quanteda corpuses and performs some descriptive stats on texts
## It needs only to be executed once

rm(list = ls(all = TRUE))

library(quanteda)
library(readtext)
library(tidyr)
library(dplyr)
library(multidplyr)
library(purrr)
library(stringr)
library(stringi)

## Use exact values instead of scientific notations
options(scipen=999)

## --- Preparing text for analysis ---
cand_tweets = readtext("data/cand_campaign_tweets.csv", textfield = "text")
parties_tweets = readtext("data/parties_campaign_tweets.csv", textfield = "text")

## Create a quanteda corpus
twCorpus = corpus(cand_tweets)
twCorpus$documents$name = tolower(twCorpus$documents$name)
ptwCorpus = corpus(parties_tweets)
ptwCorpus$documents$name = tolower(ptwCorpus$documents$name)

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

    corpus_texts = stri_trans_general(corpus_texts, "Latin-ASCII")
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
summary(ntoken(twCorpus))

## Arguments to tokenize can be passed 
## ntoken(twCorpus, remove_punct = TRUE)

## How many types (unique words)
barplot(ntype(tolower(twCorpus)))

## Candidates that are to be removed
## removed = twCorpus$documents$name[ntype(tolower(twCorpus)) < 50]

## Keep only documents with more than 40 types
twCorpus = corpus_subset(twCorpus, ntype(tolower(twCorpus)) > 50)

## Keywords in context
kwic_hamon = kwic(tolower(twCorpus$documents$text), "hamon", window = 3)
kwic_marché = rbind(kwic(tolower(twCorpus$documents$text), "marche", window = 3),View(kwic(tolower(ptwCorpus$documents$text), "marche", window = 3))

## Save corpuses as R objects
save(twCorpus, ptwCorpus, file = "data/corpus.Rdata")
