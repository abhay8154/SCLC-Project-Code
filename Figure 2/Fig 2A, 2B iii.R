#Fig 2A, 2B iii.
#Sample Clustering

#data = dataset containing genes in rows and samples in columns. 
#Based on k-means clustering for top 3000 most variable genes for each dataset

library(factoextra)
library(ggpubr)
library(stringr)
library(xlsx)

#Perform clustering 
data<-read.csv("Datasets/SCLC_GSE73160.csv",row.names = 1)
var_genes=apply(data,1,var)
data <- data[rev(order(var_genes))[1:3000],]
data <- as.data.frame(t(data))
data <- scale(data) 
#top 3000 genes based on variance
set.seed(6)
b<-kmeans(data,5)
a <- fviz_cluster(kmeans(data,5),data, geom = "point",
                  xlab = "PC1", ylab = "PC2",ggtheme = theme_pubr(), labelsize = 16)
plot(a)

#Saving individual samples as clusters

cluster_annotation<-b$cluster
cluster_annotation<-as.matrix(cluster_annotation)
table(cluster_annotation)

for (i in 1:length(table(cluster_annotation)))
{
  
  write.xlsx(row.names(cluster_annotation)[which(cluster_annotation==i)],
             "Sample_Clusters.xlsx",sheetName = paste("cluster",i),append=TRUE)
}

