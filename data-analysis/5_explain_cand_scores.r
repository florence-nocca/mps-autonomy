## 5_explain_cand_scores.r examines the correlation between candidates different similarity scores from previous models and socio-pol variables 

rm(list = ls(all = TRUE))

library(knitr)
library(caret)
library(gmodels)
library(lattice)
library(ggplot2)
library(gridExtra)
library(Kmisc)
library(ROCR)
library(corrplot)

## Use exact values instead of scientific notations
options(scipen=999)

## Read candidates data
cand_data = read.csv("tweets/cand_scores.csv", header = TRUE, na.strings=c(""))


## Round wordscores value
cand_data$wordscores = round(cand_data$wordscores, digits = 1)
cand_data$wordscores = round(cand_data$wordscores, digits = 0)


## Select only candidates from main parties
cand_data = cand_data[cand_data$nuance == "SOC" | cand_data$nuance == "LR" | cand_data$nuance == "REM" | cand_data$nuance == "UDI",]

cand_data = droplevels(cand_data)

## --- Exploratory Analysis ---
set.seed(1023)

## Looking at data structure
str(cand_data)

cand_data_subset = subset(cand_data, select = c(account,sex,naissance,nuance,wordscores))
colnames(cand_data_subset)

cand_data_subset$wordscores = as.factor(cand_data_subset$wordscores)

cand_data_subset$naissance = as.factor(cand_data_subset$naissance)

cand_data_subset$sex = as.factor(cand_data_subset$sex)

kable(head(cand_data_subset))

## Counting NAs
cols_withNa = apply(cand_data_subset, 2, function(x) sum(is.na(x)))
## No NA in the dataset

## --- Categorical Variable Analysis ---
factor_vars = names(which(sapply(cand_data_subset, class) == "factor"))
factor_vars = setdiff(factor_vars, "account")
factor_vars = setdiff(factor_vars, "wordscores")

chisq_test_res = lapply(factor_vars, function(x) { 
    chisq.test(cand_data_subset[,x], cand_data_subset[, "wordscores"], simulate.p.value = TRUE)
})

names(chisq_test_res) = factor_vars

## Chi-squared p-value results tell us that wordscores values do not depend upon sex nor naissance (as p.value > 0.05) but they depend on wordscores 
chisq_test_res

## Significativeness of given categorical variables with barcharts with the cross table row proportions as input
barchart_res = lapply(factor_vars, function(x) { 
    title = colnames(cand_data_subset[,x, drop=FALSE])
    wgd = CrossTable(cand_data_subset[,x], cand_data_subset$wordscores, prop.chisq=F)
    barchart(wgd$prop.row, stack=F, auto.key=list(rectangles = TRUE, space = "top", title = title))
})

names(barchart_res) = factor_vars
barchart_res$nuance

## Descriptive statistics
table(cand_data_subset$wordscores, cand_data_subset$nuance)

## Logistic regression
training.data.raw = cand_data

sapply(training.data.raw,function(x) sum(is.na(x)))

sapply(training.data.raw, function(x) length(unique(x)))

## Missing data
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")

## Removing variables with too many na
data = subset(training.data.raw,select=-c(delu1,elue2))
missmap(data, main = "Missing values vs observed")

library(nnet)
data$nuance = relevel(data$nuance, ref = "REM")
mod = multinom(wordscores ~ nuance, data)
predict(mod,data,"probs")

## Odds (antilog the coeff with exp function)
exp(coef(mod))

library(stargazer)
stargazer(mod, title="Regression", align=TRUE)

