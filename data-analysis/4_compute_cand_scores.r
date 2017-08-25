## 4_compute_cand_scores.r transforms the corpuses into dfms and performs different models on them to compute candidates and parties similarity
## It needs to be executed again when a new score is computed to append it to cand_scores.csv

rm(list = ls(all = TRUE))

library(quanteda)
library(readtext)
library(stringi)
library(ggplot2)
library(cowplot)
## library(graphics) # For dendrogram
## library(ape) # For dendrogram

## Use exact values instead of scientific notations
options(scipen=999)

## Load corpuses
load("data/corpus.Rdata")
## load("data/net_corpus.Rdata")

## ## --- First execution only ---
## ## --- Retrieving candidates social and political characteristics ---
## require(readstata13)
## data = read.dta13("data/french_cand_data.dta")
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
cand_data = read.csv("data/cand_scores.csv", header = TRUE, na.strings="NA")
## --- End ---

## Indicate party's account as docname
docnames(ptwCorpus) = paste(docvars(ptwCorpus, "name"))

ptwCorpus$documents$party = c("SOC","LR","PG","PCF","FN","REM","ECO","UDI","FI")

## Select parties
ptwCorpus = corpus_subset(ptwCorpus, name == "partisocialiste" | name == "lesrepublicains" | name == "enmarchefr")
## ptwCorpus = corpus_subset(ptwCorpus, name == "partisocialiste" | name == "lesrepublicains" | name == "enmarchefr" | name == "fn_officiel" | name == "franceinsoumise" )

## Add party as twCorpus' docvar
cand_party = data.frame(party = cand_data$nuance, row.names = cand_data$account) 

twCorpus = corpus_subset(twCorpus, name %in% as.character(cand_data$account))

twCorpus$documents$party = as.character(cand_party[as.character(twCorpus$documents$name),])

## twCorpus = corpus_subset(twCorpus, party %in% c("SOC","REM","LR"))

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
docvars(ptwCorpus, "docset") = 1 
docvars(twCorpus, "docset") = 2
allCorpus = ptwCorpus + twCorpus

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
    scores = c(ref_scores, rep(NA, ndoc(virgin_docs)))
    ws = textmodel_wordscores(dfm, scores)
    pred = predict(ws)
    return (pred)
}

## model_1 = predictWordscores(alldfm, twCorpus, ref_scores = c(3.83, 7.67, 9.64, 5.91, 1.7))
model_1 = predictWordscores(alldfm, twCorpus, ref_scores = c(3.83, 7.67, 5.91))

## Scores predicted by the model
scores = model_1@textscores$textscore_raw
std_err = model_1@textscores$textscore_raw_se

## Differentiate scores from mps and parties
parties_scores = scores[1:ndoc(ptwCorpus)]
mps_scores = scores[(ndoc(ptwCorpus)+1):length(scores)]
mps_std_err = std_err[(ndoc(ptwCorpus)+1):length(std_err)]

## Create empty data frame
## mps_scores = data.frame(account = twCorpus$documents$name, wordscores = mps_scores, stringsAsFactors = FALSE)
mps_scores = data.frame(account = twCorpus$documents$name, ws_se = mps_std_err, stringsAsFactors = FALSE)

## Remove previously computed wordscores
## cand_data = subset(cand_data, select = -wordscores)

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

## Legend
pnames = c("SOC","LR","REM")

## Save plot as png
## png("Graphs/Plot_wordscores_french_cand.png", width=width * ratio, height=height * ratio, res=dpi)
pdf("Graphs/Plot_wordscores_french_cand.pdf", width=width * ratio, height=height * ratio, res=dpi)
x = mps_scores$wordscores
plot(x=x, y=1:len, xlim=c(min(parties_scores,x), max(parties_scores,x)), xlab="Scores sur une échelle gauche-droite", ylab="Index des comptes", col=colors, main="", cex.main=1.5, pch=1, cex=0)
text(x=x, y=1:length(labels), labels=labels, cex=0.5, col=colors)
abline(v=parties_scores, col=as.character(parties_colors[ptwCorpus$documents$name,]))
legend(x=6.4, y=30, pnames, fill=as.character(parties_colors[ptwCorpus$documents$name,]), cex=0.8)
dev.off()

## Histograms by party
## Save histograms as pdf
pdf("Graphs/Hist_wordscores_french_cand.pdf")
par(mfrow=c(3,1), oma = c(0, 0, 0, 0))
lapply(1:length(pnames), function(n) {
    x = cand_data$wordscores[cand_data$wordscores>0][cand_data$nuance == pnames[n]]
    hist(x = x, breaks=seq(4.5,7,by=0.1), xlim=c(4.5,7),main=paste0(pnames[n]," (n = ",length(x),")"), xlab="", ylab="")
    abline(v=parties_scores[n], col=as.character(as.data.frame(parties_colors)$colors[n]))
})
dev.off()

## NOMINATE and wordscores
## Add nominatepr
## require(readstata13)
## database = read.dta13("data/DonneesRFS.dta")
## colnames(database) = tolower(colnames(database))
## to_match = data.frame(account = tolower(cand_data$account))
## col_matched = data.frame(account = database$compte, nom2d1 = database$nominate_1d, nom2d2 = database$nominate_2d)
## cand_data = merge(x=cand_data, y=col_matched, by='account')
## ## Write changes to cand_scores file
## write.csv(cand_data, "data/cand_scores.csv", row.names = FALSE)

## ## Add membres of parliamentary group "Constructifs" to database
## constructifs = data.frame(account = cand_data$account, constructif = rep(0, length(cand_data$account)))

## to_add = c("marinebrenier","antoineherth","vincentledoux59","morelpierre","franckriester","solere92","warsmann")

## constructifs[constructifs$account %in% to_add,]$constructif = 1
## cand_data = merge(cand_data, constructifs, by = "account")
## ## Write changes to cand_scores file
## write.csv(cand_data, "data/cand_scores.csv", row.names = FALSE)

## Plot NOMINATE and wordscores
to_be_removed = which(is.na(cand_data$nom2d1))
data = cand_data[-to_be_removed,]

x = data$wordscores
y = data$nom2d1

data$nuance = as.character(data$nuance)
data[data$frondeur == 1,]$nuance = gsub("SOC","Frondeur",data[data$frondeur == 1,]$nuance)
data[data$constructif == 1,]$nuance = gsub("LR","Constructif",data[data$constructif == 1,]$nuance)

z = as.factor(data$nuance)
Palette = c("cyan","black","blue","gold1","red")

p = ggplot(data, aes(x, y, colour=factor(z), label = data$account)) + 
    geom_point(## size=3, shape=19
    ) + labs(x = "Wordscores", y = "Nominate (dimension 1)") +  scale_colour_manual(values=Palette) + labs(colour='Parti') + geom_text(x = 6.5, y = 1, label = "LR", colour = "blue", size = 3, vjust = 1) + geom_text(x = min(x), y = min(y), label = "SOC", colour = "red", size = 3) + geom_text(x = 5.82, y = 0, label = "REM", colour = "gold", size = 3, vjust = 2) + theme_classic() + theme(legend.key = element_rect(colour = '#bdbdbd', size = 0.6) )

## To add labels
## + geom_text(aes(label = data$account), hjust = 0, vjust = 0, size = 5)

save_plot("Graphs/Plot_NOM_wordscores_french_cand.pdf", p, base_height = 6, base_width = 7) 

## --- Wordfish model ---
predictWordfish = function(twCorpus, ptwCorpus, cand_party, parties_to_keep)
{

    subtwCorpus = corpus_subset(twCorpus, party %in% cand_party)
    subptwCorpus = corpus_subset(ptwCorpus, name %in% parties_to_keep)
    allCorpus = subtwCorpus + subptwCorpus
    alldfm = to_dfm(allCorpus)
    alldfm = dfm_wordstem(alldfm, language = "french")

    len_corpus = length(subtwCorpus$documents$name)

    wf = textmodel_wordfish(alldfm, dir = c(len_corpus + 1, len_corpus + 2))
    return (wf)
}

wfm = predictWordfish(twCorpus, ptwCorpus, c("REM","SOC"), c("enmarchefr", "partisocialiste"))
textplot_scale1d(wfm)

## ------ Machine Learning ------

## --- Compute cosine similarity ---
## simil = as.matrix(textstat_simil(dfm_weight(alldfm, "relFreq"), c("partisocialiste", "lesrepublicains","enmarchefr","franceinsoumise","fn_officiel","udi_off","eelv")), margin = "documents", method = "cosine")
simil = as.matrix(textstat_simil(dfm_weight(alldfm, "relFreq"), c("partisocialiste", "lesrepublicains","enmarchefr")), margin = "documents", method = "cosine")

## Set minimum value to 0
simil = pmax(simil, 0)

simil = as.data.frame(simil)[-c(1,2,3),]
simil = as.data.frame(simil)

## colnames(simil) = gsub("^","net_",colnames(simil))

simil$account = twCorpus$documents$name
    
cand_data = merge(cand_data, simil, by = "account")

## cand_data = subset(cand_data, select = -(73:76))

## --- k-means --- 
simil = subset(simil, select = -account)

k = 3

km_simil = kmeans(simil, k, algorithm = "Hartigan-Wong", nstart=100, iter.max = 100)

## Find each party's class number looking at centers
km_simil$centers

classes = lapply(1:k, function(n){
    unique(twCorpus$documents[km_simil$cluster == n,]$name)
})

## Look at class composition
n = 1
table(cand_data[cand_data$account %in% classes[[n]],]$nuance)
## k = 3 with tweets' content does not find the 3 parties, but k = 4 does, with a fourth "unclassable" class

## Create empty data frame
km_on_simil = data.frame(account = character(), net_km_simil = numeric(), stringsAsFactors = FALSE)

## Append data frame with accounts and their corresponding class
for(i in 1:k){
    km_on_simil = rbind(km_on_simil, data.frame(account = tolower(classes[[i]]), net_km_simil = rep(i, length(classes[[i]]))))
}

cand_data = merge(cand_data, km_on_simil, by = "account")

## km_on_simil$km_simil = pred_party

parties_classes = data.frame(party = c("SOC","LR","REM"), row.names = c(1, 2,3))

pred_party = as.character(parties_classes[cand_data$net_km_simil,])

## cand_data$km_simil = (pred_party != cand_data$nuance)

cand_data$net_km_pred = pred_party

cand_data = subset(cand_data, select = -net_km_simil)

## --- Naive Bayesian ---

## First run: party document as training data
## trainingset = dfm_subset(alldfm, docset == 1)
## predictionset = dfm_subset(alldfm, docset == 2)
## trainingclass = factor(allCorpus$documents$party[1:3])

## Other runs: parties and "alwaysloyalists" as training data
trainingset = dfm_subset(alldfm, name %in% c(always_loyalists,ptwCorpus$documents$name))
predictionset = dfm_subset(alldfm, docset == 2)
trainingclass = factor(allCorpus$documents[allCorpus$documents$name %in% c(always_loyalists,ptwCorpus$documents$name),]$party)

model = textmodel_NB(trainingset, trainingclass)

## Party prediction
success = unlist(lapply(1:length(twCorpus$documents$name), function(n){

    prediction = predict(model, newdata = predictionset[n])
    pred_party = prediction$nb.predicted
    true_party = twCorpus$documents$party[n]
    ## return(pred_party == true_party)
    return(pred_party)
})
)

net_naive_pred_bis = success
## success_rate = sum(success) / length(twCorpus$documents$text)
## success rate of 79\% (92 with networks) on first run
## 85\% (89 for networks) with always_loyalists as training data

## Party predicted
naive_lr = twCorpus$documents$name[which(net_naive_pred_bis == "LR")]
naive_soc = twCorpus$documents$name[which(net_naive_pred_bis == "SOC")]
naive_rem = twCorpus$documents$name[which(net_naive_pred_bis == "REM")]

## Merge by account names
naive_party_pred = data.frame(account = naive_lr, net_naive_pred_bis = "LR")
naive_party_pred = rbind(naive_party_pred, data.frame(account = naive_soc, net_naive_pred_bis = "SOC"))
naive_party_pred = rbind(naive_party_pred, data.frame(account = naive_rem, net_naive_pred_bis = "REM"))

cand_data = merge(cand_data, naive_party_pred, by = "account")

## Remove previously computed naive
## cand_data = subset(cand_data, select = -naive_diss)

## Merge by account names
## naive = data.frame(account = naive_dissidents, naive_diss = TRUE)
## naive = rbind(naive, data.frame(account = naive_loyalists, naive_diss = FALSE))

## cand_data = merge(cand_data, naive, by = "account")

## --- Wordshoal ---
wordshoalfit = textmodel_wordshoal(alldfm, dir = c(1,2),
                        groups = docvars(allCorpus, "party"), 
                        authors = docvars(allCorpus, "name"))

fitdf = merge(as.data.frame(summary(wordshoalfit)),
               docvars(allCorpus), 
               by.x="row.names", by.y="name")

fitdf = subset(fitdf,duplicated(party))

aggregate(theta ~ party, data = fitdf, mean)

par(mfrow=c(3,1), oma = c(0, 0, 0, 0))
lapply(ptwCorpus$documents$party, function(party){
hist(as.numeric(fitdf[fitdf$party == party,]$theta), main = party)
}
)

## --- SVM ---
library(e1071)

model = svm(trainingset,trainingclass, kernel = "sigmoid")
pred = predict(model, predictionset)
pred = as.character(pred)
table(pred, twCorpus$documents$party)
## First run: success rate of 78\% (84 on networks))
# Following runs: 85\% (59 on networks !)

success = unlist(lapply(1:length(twCorpus$documents$name), function(n){

    pred_party = pred[n]
    true_party = twCorpus$documents$party[n]
    return(pred_party == true_party)    
})
)
success_rate = sum(success) / length(twCorpus$documents$text)

## Party predicted
svm_lr = twCorpus$documents$name[which(pred == "LR")]
svm_soc = twCorpus$documents$name[which(pred == "SOC")]
svm_rem = twCorpus$documents$name[which(pred == "REM")]

## Merge by account names
svm_party_pred = data.frame(account = svm_lr, net_svm_pred = "LR")
svm_party_pred = rbind(svm_party_pred, data.frame(account = svm_soc, net_svm_pred = "SOC"))
svm_party_pred = rbind(svm_party_pred, data.frame(account = svm_rem, net_svm_pred = "REM"))

cand_data = merge(cand_data, svm_party_pred, by = "account")

## Rerun models with loyalists as training data
svm_loyalists = cand_data[cand_data$nuance == cand_data$net_svm_pred,]$account
naive_loyalists = cand_data[cand_data$nuance == cand_data$net_naive_pred,]$account
## km_pred = gsub("NA",as.character("NA"),cand_data$km_pred)
km_loyalists = cand_data[cand_data$nuance == cand_data$net_km_pred,]$account

always_loyalists = Reduce(intersect, list(svm_loyalists,naive_loyalists,km_loyalists))

## mps_predicted = simil[!(simil$account %in% c(always_loyalists,ptwCorpus$documents$name)),]$account

## --- KNN ---
library(class)

## KNN on cosine similarity does not work well (62\%)
## train_seq = which(simil$account %in% always_loyalists)
## test_seq = 4:length(simil$account)

## train = simil[c(1:3,train_seq),1:3]
## labels = allCorpus$documents[c(1:3,train_seq),]$party
## test = simil[test_seq,1:3]

## pred = knn(train,test,labels,3)

## KNN on dfm is better (79\%, but still less than other models with always_loyalists training data) (74 with networks)
pred = knn(trainingset,predictionset,trainingclass,3)

success = unlist(lapply(1:length(twCorpus$documents$name), function(n){

    pred_party = pred[n]
    true_party = twCorpus$documents$party[n]
    ## return(pred_party == true_party)
    return(pred_party)    
})
)
## success_rate = sum(success) / length(twCorpus$documents$text)

knn_lr = twCorpus$documents$name[which(pred == "LR")]
knn_soc = twCorpus$documents$name[which(pred == "SOC")]
knn_rem = twCorpus$documents$name[which(pred == "REM")]

## Merge by account names
knn_party_pred = data.frame(account = knn_lr, net_knn_pred_bis = "LR")
knn_party_pred = rbind(knn_party_pred, data.frame(account = knn_soc, net_knn_pred_bis = "SOC"))
knn_party_pred = rbind(knn_party_pred, data.frame(account = knn_rem, net_knn_pred_bis = "REM"))

cand_data = merge(cand_data, knn_party_pred, by = "account")

## Write changes to cand_scores file
write.csv(cand_data, "data/cand_scores.csv", row.names = FALSE)

## Detect systematic dissidents
svm_dissidents = cand_data[cand_data$nuance != cand_data$svm_pred,]$account
naive_dissidents = cand_data[cand_data$nuance != cand_data$naive_pred,]$account

## cand_data$km_pred = gsub("NA",as.character("NA"),cand_data$km_pred)
km_dissidents = cand_data[as.character(cand_data$nuance) != cand_data$km_pred,]$account

always_dissidents = Reduce(intersect, list(svm_dissidents,naive_dissidents,km_dissidents))

both_dissidents = Reduce(intersect, list(net_always_dissidents,always_dissidents))

svm_bis_dissidents = cand_data[as.character(cand_data$nuance) != as.character(cand_data$svm_pred_bis),]$account

naive_bis_dissidents = cand_data[as.character(cand_data$nuance) != as.character(cand_data$naive_pred_bis),]$account

knn_bis_dissidents = cand_data[as.character(cand_data$nuance) != as.character(cand_data$knn_pred_bis),]$account


