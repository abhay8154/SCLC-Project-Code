#Fig S2A, S2B i-v & S3B, S3D 

library(cmapR)
library(CePa)
library(xlsx)
library(dplyr)
library(rstatix)
library(ggpubr)

#GCT file creation
data<-read.csv("Datasets/SCLC_GSE73160.csv",row.names = 1)
data<-as.matrix(data)
gct_sclc<-GCT(data,rid=row.names(data),cid=colnames(data))
write_gct(gct_sclc,"gct_GSE73160.gct")

#Read sample clusters
Clusters_1<-read.xlsx("Sample_Clusters.xlsx",sheetIndex = 1)
Clusters_1<-Clusters_1$x
Clusters_2<-read.xlsx("Sample_Clusters.xlsx",sheetIndex = 2)
Clusters_2<-Clusters_2$x
Clusters_3<-read.xlsx("Sample_Clusters.xlsx",sheetIndex = 3)
Clusters_3<-Clusters_3$x
Clusters_4<-read.xlsx("Sample_Clusters.xlsx",sheetIndex = 4)
Clusters_4<-Clusters_4$x
Clusters_5<-read.xlsx("Sample_Clusters.xlsx",sheetIndex = 5)
Clusters_5<-Clusters_5$x

#Subset Clusterwise ssGSEA scores
clus1_ssgsea<-gsea.score[,Clusters_1]
clus2_ssgsea<-gsea.score[,Clusters_2]
clus3_ssgsea<-gsea.score[,Clusters_3]
clus4_ssgsea<-gsea.score[,Clusters_4]
clus5_ssgsea<-gsea.score[,Clusters_5]
ssGSEA_global<-cbind(clus1_ssgsea,clus2_ssgsea,clus3_ssgsea,clus4_ssgsea,clus5_ssgsea)
write.xlsx(ssGSEA_global,"GSE73160_ssGSEA_scores_global.xlsx")

clus_1<-as.matrix(rowMeans(clus1_ssgsea))
clus_2<-as.matrix(rowMeans(clus2_ssgsea))
clus_3<-as.matrix(rowMeans(clus3_ssgsea))
clus_4<-as.matrix(rowMeans(clus4_ssgsea))
clus_5<-as.matrix(rowMeans(clus5_ssgsea))

#Gene dendrogram
ssgsea_clus<-cbind(ssgsea_clus,clus_3)
ssgsea_clus<-cbind(ssgsea_clus,clus_4)
ssgsea_clus<-cbind(ssgsea_clus,clus_5)
row.names(ssgsea_clus)<-c("Mixed","ASCL1","NEUROD1","POU2F3","YAP1")
a<-hclust(dist(ssgsea_clus))
plot(a,main="GSE73160 Gene Set Dendrogram",
     xlab = "",sub = "",cex=3,cex.lab=1.5,cex.main=3,cex.axis=2.5) #Fig S3B, S3D 

col.names<-c("Cluster_1","Cluster_2","Cluster_3","Cluster_4","Cluster_5")
colnames(ssgsea_clus)<-col.names

write.xlsx(ssgsea_clus,"mean_ssGSEA_scores.xlsx")

df<-t(clus1_ssgsea)
c1<-replicate(14,"Cluster_1")
df<-cbind(df,c1)
c2<-replicate(33,"Cluster_2")
temp<-cbind(t(clus2_ssgsea),c2)
df<-rbind(df,temp)
c3<-replicate(5,"Cluster_3")
temp<-cbind(t(clus3_ssgsea),c3)
df<-rbind(df,temp)
c4<-replicate(10,"Cluster_4")
temp<-cbind(t(clus4_ssgsea),c4)
df<-rbind(df,temp)
c5<-replicate(5,"Cluster_5")
temp<-cbind(t(clus5_ssgsea),c5)
df<-rbind(df,temp)
df<-as.data.frame(df)
colnames(df)<-c("Mixed","ASCL1","NEUROD1","POU2F3","YAP1","Clusters")

write.xlsx(df,"ssGSEA_scores_global.xlsx")
df<-read.xlsx("ssGSEA_scores_global.xlsx",sheetIndex = 1)
rownames(df) = df[, 1]
df = df[, -1] 

#Fig S2A, S2B i-v

#Welch's t-test with fdr correction for multiple comparisons
jpeg("SCLC A.jpeg", width = 1400, height = 1000)
temp_data=df
temp_data$Clusters <- factor(temp_data$Clusters, levels = c("Cluster 1","Cluster 2","Cluster 3",
                                                            "Cluster 4","Cluster 5"))

t_test<-temp_data %>% t_test(ASCL1 ~ Clusters,var.equal=FALSE) %>% adjust_pvalue(method = "fdr" )
t_test<-add_significance(t_test)

stat.test <- t_test %>%
  add_xy_position(x = "Clusters", dodge = 0.8)
Graph1=ggplot(temp_data,aes(y=ASCL1,x=Clusters))
Graph1+
  stat_summary(fun = mean,
               geom="bar",fill = "steelblue", color = "black")+
  stat_summary(fun.data = mean_se,
               geom="errorbar",
               width=.2)+
  stat_pvalue_manual(stat.test,
                     label = "p.adj.signif" , 
                     bracket.size = 3, size = 18 )+
  border()+
  theme(text = element_text(size=60, face="bold"), axis.title.x=element_text(size=60,face="bold"),
        axis.title.y=element_text(size=60,face="bold"))+
  ylab("SCLC-A")
dev.off()

write.xlsx(t_test,"GSE73160_Welch_t-test_results.xlsx",
           sheetName = "Gene_Set_5",append = TRUE)
