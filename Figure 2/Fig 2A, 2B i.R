#Fig 2A, 2B i.
#DF = Cell line dataset, either CCLE or GSE73160 

library(corrplot)
library(ggcorrplot)
library(dendextend)
library(WGCNA)
library(readxl)
library(xlsx)

DF<-read.csv("Datasets/GSE73160.csv",row.names = 1)
genes<-as.data.frame(read_excel("Datasets/GS_105.xlsx",sheet=1))
DF_sub <- DF[genes[,2],]
COL <- genes[complete.cases(DF_sub),3]
DF_sub <- DF_sub[complete.cases(DF_sub),]
CORR <- cor(t(DF_sub) , method = "spearman")  #Correlation matrix

dendrogram = hclust(d=as.dist(1-CORR),method="average")
sizeGrWindow(7, 6)
plot(dendrogram, main = "GSE73160 Dendrogram 105 genes",
     xlab = "", sub = "",cex=0.9)

#Use k=5 (in place of h=0.73) for CCLE to get five clusters in the dendrogram 
cut_avg <- cutree(dendrogram, h=0.73) #cut the dendrogram at h=0.73
a<-as.data.frame(cut_avg)
table(a)
d <- cbind(rownames(a), data.frame(a, row.names=NULL))
clus<-data.frame()

#Gene Clusters
for (i in 1:length(table(a)))
{
  clus<-data.frame()
  clus[1:as.numeric(table(d$cut_avg==i)[2]),1]<-d[d$cut_avg==i,]$`rownames(a)`
  write.xlsx(clus[1:as.numeric(table(d$cut_avg==i)[2]),1],
             "GSE73160_dendro_clusters.xlsx",sheetName = paste("cluster",i),append=TRUE)
}
