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
cand_data$wordscores = round(cand_data$wordscores, digits = 1)

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
barchart_res$naissance
barchart_res$sex
