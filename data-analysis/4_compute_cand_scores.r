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
library(stringi)
library(graphics) # For dendrogram
library(ape) # For dendrogram

## Use exact values instead of scientific notations
options(scipen=999)

## Load corpuses
load("tweets/corpus.Rdata")

## ## --- First execution only ---
## ## --- Retrieving candidates social and political characteristics ---
## require(readstata13)
## data = read.dta13("tweets/french_cand_data.dta")
## colnames(data) = tolower(colnames(data))

## ## Correct wrongly assigned account
## index = which(tolower(data$compte) == "lecallennec")
## data$compte[index] = "ilecallennec" 
## data$compte = tolower(data$compte)

## ## Keep only candidates mps
## data = data[data$represente == 1,]

## ## Replace starting date by starting year only
## data$debutmandat = as.numeric(unlist(regmatches(as.character(data$debutmandat)
## , regexec("^[^-]+", as.character(data$debutmandat)
## ))))

## data$naissance = as.numeric(unlist(regmatches(as.character(data$naissance)
## , regexec("^[^-]+", as.character(data$naissance)
## ))))

## ## Keep only candidates from twCorpus
## to_match = data.frame(account = tolower(twCorpus$documents$name))
## cand_data = merge(x=to_match, y=data, by.x='account', by.y='compte')
## ## --- End of first execution ---

## --- Not first execution ---
## Read database on mps, making sure that the parameter na.strings is equal to c("") so that each missing value is coded as a NA
cand_data = read.csv("tweets/cand_scores.csv", header = TRUE, na.strings=c(""))
## --- End ---

## Select parties
ptwCorpus = corpus_subset(ptwCorpus, name == "partisocialiste" | name == "lesrepublicains" | name == "enmarchefr")
## ptwCorpus = corpus_subset(ptwCorpus, name == "partisocialiste" | name == "lesrepublicains" | name == "enmarchefr" | name == "fn_officiel" | name == "franceinsoumise" )

## Indicate party's account as docname
docnames(ptwCorpus) = paste(docvars(ptwCorpus, "name"))

## Add party as twCorpus' docvar
cand_party = data.frame(party = cand_data$nuance, row.names = cand_data$account) 

twCorpus = corpus_subset(twCorpus, name %in% as.character(cand_data$account))

twCorpus$documents$party = as.character(cand_party[as.character(twCorpus$documents$name),])

twCorpus = corpus_subset(twCorpus, party %in% c("SOC","REM","LR"))

## Indicate name and party as docname
docnames(twCorpus) = paste(docvars(twCorpus, "name"), docvars(twCorpus, "party"))

## Add additional stopwords
my_stopwords = as.vector(unlist(read.table("stopwords-fr.txt")))
my_stopwords = stri_trans_general(c(my_stopwords, "faire", "faut", "veux","veut","oui","non"), "Latin-ASCII")

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

## --- Compute cosine similarity ---
simil = as.matrix(textstat_simil(dfm_weight(alldfm, "relFreq"), c("partisocialiste", "lesrepublicains","enmarchefr","franceinsoumise","fn_officiel","udi_off","eelv")), margin = "documents", method = "cosine")

## Set minimum value to 0
simil = pmax(simil, 0)

simil = as.data.frame(simil)[-c(1,2,3,4,5,6,7,244,245),]

simil$account = twCorpus$documents$name

cand_data = merge(cand_data, simil, by = "account")

## --- Wordscores model ---
predictWordscores = function(dfm, virgin_docs, ref_scores)
{
    scores = c(rep(NA, ndoc(virgin_docs)), ref_scores)
    ws = textmodel_wordscores(dfm, scores)
    pred = predict(ws)
    return (pred)
}

## model_1 = predictWordscores(alldfm, twCorpus, ref_scores = c(3.83, 7.67, 9.64, 5.91, 1.7))
model_1 = predictWordscores(alldfm, twCorpus, ref_scores = c(3.83, 7.67, 5.91))

## Scores predicted by the model
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

### Graphical representations
## Graphical parameters
## parties_colors = data.frame(colors = c("red", "blue", "yellow"), row.names = ptwCorpus$documents$name)
## parties_colors = data.frame(colors = c("red", "blue", "black","yellow","indianred"), row.names = ptwCorpus$documents$name)
parties_colors = data.frame(colors = c("red", "blue","yellow"), row.names = ptwCorpus$documents$name)

## cand_colors = data.frame(colors = c("blue", "red", rep("gray",4), "yellow", rep("gray", 6)), row.names = unique(cand_data$nuance)) 
cand_colors = data.frame(colors = c("red", "yellow", "blue"), row.names = unique(twCorpus$documents$party)) 

colors = as.character(cand_colors[as.character(twCorpus$documents$party),])

## Resolution options for plot
width = 1300 * 0.7
height = 768 * 0.7
dpi = 200
ratio = dpi / 72

### Plot
## y-axis
len = length(mps_scores$wordscores)

## Labels
labels = twCorpus$documents$name

## Save plot as png
png("Graphs/Plot_wordscores_french_cand.png", width=width * ratio, height=height * ratio, res=dpi)
x = mps_scores$wordscores
plot(x=x, y=1:len, xlim=c(min(parties_scores,x), max(parties_scores,x)), xlab="Scores sur une échelle gauche-droite", ylab="Index des comptes", col=colors, main="Positionnement des candidats par rapport aux comptes des partis (LR, PS, REM)", cex.main=1.5, pch=1, cex=0)
text(x=x, y=1:length(labels), labels=labels, cex=0.5, col=colors)
abline(v=parties_scores, col=as.character(parties_colors[ptwCorpus$documents$name,]))
legend(x=6.4, y=30, unique(ptwCorpus$documents$name), fill=as.character(parties_colors[ptwCorpus$documents$name,]), cex=0.8)
dev.off()

## Histograms by party
pnames = c("SOC","LR","REM")

## Save histograms as pdf
pdf("Graphs/Hist_wordscores_french_cand.pdf")
par(mfrow=c(3,1), oma = c(0, 0, 0, 0))
lapply(1:length(pnames), function(n) {
    x = cand_data$wordscores[cand_data$wordscores>0][cand_data$nuance == pnames[n]]
    hist(x = x, breaks=seq(4.5,7,by=0.1), xlim=c(4.5,7),main=paste0(pnames[n]," (n = ",length(x),")"), xlab="", ylab="")
    abline(v=parties_scores[n], col=as.character(as.data.frame(parties_colors)$colors[n]))
})
dev.off()

## --- Wordfish model ---
twCorpus = corpus_subset(twCorpus, party %in% c("SOC","LR")
ptwCorpus = corpus_subset(ptwCorpus, name %in% c("partisocialiste", "lesrepublicains"))
allCorpus = twCorpus + ptwCorpus
alldfm = to_dfm(allCorpus)
alldfm = dfm_wordstem(alldfm, language = "french")

wfm1 = textmodel_wordfish(alldfm, dir = c(120,121))

textplot_scale1d(wfm1)

write.csv(cand_data, "tweets/cand_scores.csv", row.names = FALSE)
