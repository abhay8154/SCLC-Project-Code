#Fig 2A, 2B v. & S3A,C

library(readxl)
library(ggcorrplot)
library(corrplot)
library(WGCNA)
library(dendextend)
library(xlsx)
suppressPackageStartupMessages(library(dendextend))


DF_sub <- read.xlsx("Datasets/GSE73160_Combined_matrix.xlsx",sheetIndex = 1)
rownames(DF_sub) = DF_sub[, 1]
DF_sub = DF_sub[, -1]

CORR <- cor(t(DF_sub) , method = "spearman")  #Correlation matrix
CORR.p <- cor_pmat(t(DF_sub), method = "spearman" )
cols <- colorRampPalette(c('blue', 'white', 'red'))
jpeg(paste0("Plots/","CCLE_circle.jpeg"),height = 1080,width = 1620)
p<-corrplot(CORR,method = "circle",
            hc.order = FALSE,
            type = "lower",
            outline.color = "white", p.mat = as.matrix(CORR.p), sig.level = 0.05, insig = "pch",  #
            show.diag = TRUE, show.legend = FALSE,  tl.cex = 3.5, pch.cex = 2, tl.srt=45,
            order = "hclust",
            col = cols(256), title ="CCLE",mar=c(0,0,1,0),cl.cex = 3,tl.col="black")
dev.off()

dendrogram = hclust(d=as.dist(1-CORR),method="average")
sizeGrWindow(7, 6)
plot(dendrogram, main = "CCLE 11*11 Dendrogram",
     xlab = "", sub = "",cex=3,cex.main=3,cex.lab=1.5,cex.axis=2.5)

write.xlsx(CORR,"Correlation matrix.xlsx")

