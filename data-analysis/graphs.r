library(gridExtra)

cand_data = read.csv("data/cand_scores.csv", header = TRUE, na.strings = "NA")

## Remove NA values
to_be_removed = unique(c(which(is.na(cand_data$nom2d1)),which(is.na(cand_data$nom2d2))))

data = cand_data[-to_be_removed,]

x = data$nom2d1
y = data$nom2d2

## NOMINATE
z = as.factor(data$nuance)

Palette = c("blue","gold1","red")
nom = ggplot(data, aes(x, y, colour = factor(z), label = data$account)) + geom_point(size = 1) + labs(x = "", y = "Dimension 2") + labs(title = "NOMINATE") +  scale_colour_manual(values=Palette) + labs(colour='Parti réel') + theme_classic() + theme(plot.margin = unit(c(6,0,6,0), "pt"))

## K-means
NAs = which(is.na(data$km_pred))
data$km_pred = as.character(data$km_pred)
data$km_pred[NAs] = "Non classé"
z_km = as.factor(data$km_pred)

Palette_km= c("blue","black","gold1","red")
km = ggplot(data, aes(x, y, colour = factor(z_km), label = data$account)) + geom_point(size = 1) + labs(x = "", y = "Dimension 2") + labs(title = "k-means") +  scale_colour_manual(values=Palette_km) + labs(colour='Parti prédit') + theme_classic() + theme(plot.margin = unit(c(6,0,6,0), "pt"))

## KNN
z_knn = as.factor(data$knn_pred)

Palette = c("blue","gold1","red")
knn = ggplot(data, aes(x, y, colour = factor(z_knn), label = data$account)) + geom_point(size = 1) + labs(x = "", y = "") + labs(title = "KNN") +  scale_colour_manual(values=Palette) + labs(colour='Parti prédit') + theme_classic() + theme(plot.margin = unit(c(6,0,6,0), "pt"))

## SVM
z_svm= as.factor(data$svm_pred)

svm = ggplot(data, aes(x, y, colour = factor(z_svm), label = data$account)) + geom_point(size = 1) + labs(x = "Dimension 1", y = "") + labs(title = "SVM") +  scale_colour_manual(values=Palette) + labs(colour='Parti prédit') + theme_classic() + theme(plot.margin = unit(c(6,0,6,0), "pt")) + geom_text(aes(label = data$account), hjust = 0, vjust = 0, size = 5)

## Naive
z_naive = as.factor(data$naive_pred)

naive = ggplot(data, aes(x, y, colour = factor(z_naive), label = data$account)) + geom_point(size = 1) + labs(x = "Dimension 1", y = "Dimension 2") + labs(title = "Naive") +  scale_colour_manual(values=Palette) + labs(colour='Parti prédit') + theme_classic() + theme(plot.margin = unit(c(6,0,6,0), "pt"))

## p = grid.arrange(km, knn, svm, naive, ncol=2)
prow = plot_grid(## nom + theme(legend.position="none"),
           km + theme(legend.position="none"),
           knn + theme(legend.position="none"),
           naive + theme(legend.position="none"),
           svm + theme(legend.position="none"),
           align = 'vh',
           hjust = -1,
           nrow = 2
           )

legend = get_legend(km + theme(legend.position="bottom"))
p =  plot_grid(prow, legend, ncol = 1, rel_heights = c(1, .2))

save_plot("Graphs/Plot_NOM_ML.pdf", p, base_height = 6, base_width = 7) 
