## 6_network_analysis.r extract mentions from tweets

rm(list = ls(all = TRUE))

library(quanteda)
library(stringr)
library(readtext)

## Use exact values instead of scientific notations
options(scipen=999)

## --- Mentions Analysis ---
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

## Construct a dataframe containing for each candidate the accounts he mentioned and the number of times they are mentioned
df = lapply(1:length(twCorpus$documents$name), function(n){
mp_dfm = to_dfm(twCorpus$documents$texts[n])
dest = as.character(names(topfeatures(mp_dfm, length(mp_dfm))))
weight = as.integer(topfeatures(mp_dfm, length(mp_dfm)))
orig = rep(as.character(twCorpus$documents$name[n]), length(dest))
df = data.frame(orig = orig, dest = dest, weight = weight)
}
)

## Merge all dataframes created by lapply
df = reshape::merge_all(df)

## Sort by weight
df = df[order(-df$weight),]
df_save = df

## Minimum weight for the relation to be drawn
min = 30
df = df_save[df_save$weight >= min,]

## Accounts mentioned in the graph
accounts = data.frame(names = unique(c(as.character(df$dest),as.character(df$orig))), col = rep("gray",length(names)))

## Set a color to each account mentioned
parties_colors = data.frame(colors = c("red", "blue",rep("gray",3),"yellow",rep("gray",3)), row.names = ptwCorpus$documents$party)

accounts$col = unlist(lapply(accounts$names, function(n)
{

    if(n %in% twCorpus$documents$name)
    {
        party = twCorpus$documents[twCorpus$documents$name == n,]$party

        col = as.character(parties_colors[party,])
    }
    else
        col = "gray"
}
))

## Write dot file from df
write(paste("digraph g {\n",
                paste(as.character(df$orig),
                      "->",
                      as.character(df$dest),
                      collapse = "\n", sep=""),
                "\n",
                paste(accounts$names,
                      "[shape=circle,fontsize=12,color=",accounts$col,",style=filled,label=\"", accounts$names,"\"]",
                      collapse = "\n", sep=""), "}\n", sep=""),
          paste("Graphs/rel_graph_min",min,".dot", sep=""))

## Set presidential candidates' shape to "square" before lauching system command

## Generate pdf from dot file
system(paste("fdp -Tpdf -oGraphs/rel_graph_min",min,".pdf Graphs/rel_graph_min",min,".dot",sep=""))
