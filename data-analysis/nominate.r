library(foreign)
library(pscl)
library(MASS)
library(wnominate)
library(ggplot2)
library(cowplot)

## Generate NOMINATE
mydata = read.dta("data/Votes12.dta")
scrut = matrix(9,600,597)

for(i in 1:67955) {
	scrut[mydata[i,5],mydata[i,2]]=mydata[i,1]
    	}

data = rollcall(scrut,
             yea=1, nay=0, missing=2, notInLegis=9,
             legis.names=NULL, vote.names=NULL,
             legis.data=NULL, vote.data=NULL,
             desc=NULL, source=NULL)
         
result = wnominate(data, ubeta=15, uweights=0.5, dims=1, minvotes=20,
            lop=0.025,trials=3, polarity=324)
summary(result)

result2 = wnominate(data,polarity=c(324,17))
summary(result2)

data = result2

missing = which(is.na(data$legislators$coord1D))

data$legislators = data$legislators[-missing,]

binominate = data.frame(mps = rownames(data$legislators), nominate_1d = data$legislators$coord1D, nominate_2d = data$legislators$coord2D)

write.csv(binominate, "data/binominate.csv", row.names = FALSE)

## Adding NOMINATE to cand_scores
## require(readstata13)
## database = read.dta13("data/DonneesRFS.dta")
## colnames(database) = tolower(colnames(database))
## to_match = data.frame(account = tolower(cand_data$account))
## col_matched = data.frame(account = database$compte, frondeur = database$frondeur)
## cand_data = merge(x=cand_data, y=col_matched, by='account')
## ## Write changes to cand_scores file
## write.csv(cand_data, "data/cand_scores.csv", row.names = FALSE)

## Plot bidimensional NOMINATE
cand_data = read.csv("data/cand_scores.csv", header = TRUE, na.strings = "NA")

## Remove NA values
to_be_removed = unique(c(which(is.na(cand_data$nom2d1)),which(is.na(cand_data$nom2d2))))

data = cand_data[-to_be_removed,]

x = data$nom2d1
y = data$nom2d2
data$nuance = as.character(data$nuance)
data[data$frondeur == 1,]$nuance = gsub("SOC","Frondeur",data[data$frondeur == 1,]$nuance)

z = as.factor(data$nuance)
Palette = c("black","blue","gold1","red")

p = ggplot(data, aes(x, y, colour = factor(z), label = data$account)) + geom_point(size = 1) + labs(x = "Dimension 1", y = "Dimension 2") +  scale_colour_manual(values=Palette) + labs(colour='Parti') + theme_classic()
save_plot("Graphs/Plot_NOM_2d_french_cand.pdf", p, base_height = 6, base_width = 7) 

## To add labels
## + geom_text(aes(label = data$account), hjust = 0, vjust = 0, size = 5)
