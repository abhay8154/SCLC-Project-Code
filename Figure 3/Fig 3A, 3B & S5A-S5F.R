#Fig 3A, 3B & S5A-S5F 

#Correlation Scatter plot b/w SCLC-I* and NE
#CCLE
library(xlsx)
library(ggplot2)
library(ggpubr)
library(dplyr)
CCLE_NE<-read.xlsx("Datasets/CCLE_NE_scores.xlsx",sheetIndex = 1)
CCLE_NE<-CCLE_NE[,c(2,3)]
CCLE_SCLC_I<-read.xlsx("Datasets/SCLC I CCLE.xlsx",sheetIndex = 1)
CCLE_SCLC_I<-CCLE_SCLC_I$data
data<-cbind(CCLE_NE,CCLE_SCLC_I)
data<-as.data.frame(data)


#filter dataframe to get data to be highlighted (YAP1 cluster samples)
highlight_df <- data %>% 
  filter(Clusters=="Cluster_2")

jpeg(paste0("Correlation/","CCLE.jpeg"),height = 1080,width = 1620)
ggplot(data,aes(x=CCLE_SCLC_I ,y=data)) + 
  geom_point(shape=18, colour = "blue", aes(size = 2),show.legend=FALSE)+
  geom_point(data=highlight_df, 
             aes(x=CCLE_SCLC_I,y=data), 
             color='red',
             size=4)+
  xlab("SCLC-I*")+ ylab("NE")+
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"),axis.title.x=element_text(size=60,face="bold"),
        axis.title.y=element_text(size=60,face="bold"),
        plot.title = element_text(size = 60, face = "bold"))+
  stat_cor(method="pearson",size=16,label.x=6.5, label.y=0.8)
dev.off()

#GSE73160
#Correlation Scatter plot b/w SCLC-I* and NE

GSE73160_NE<-read.xlsx("Datasets/GSE73160_NE_scores.xlsx",sheetIndex = 1)
GSE73160_NE<-GSE73160_NE[,c(2,3)]
GSE73160_SCLC_I<-read.xlsx("Datasets/SCLC I GSE73160.xlsx",sheetIndex = 1)
GSE73160_SCLC_I<-GSE73160_SCLC_I$data
data<-cbind(GSE73160_NE,GSE73160_SCLC_I)
data<-as.data.frame(data)
data$Clusters<-as.factor(data$Clusters)

# filter dataframe to get data to be highlighted (YAP1 cluster samples)
highlight_df <- data %>% 
  filter(Clusters=="Cluster_3")

jpeg(paste0("Correlation/","GSE73160.jpeg"),height = 1080,width = 1620)
ggplot(data,aes(x=GSE73160_SCLC_I ,y=data)) + xlim(-21,-10)+
  geom_point(shape=18, colour = "blue",aes(size = 2),show.legend=FALSE)+
  geom_point(data=highlight_df, 
             aes(x=GSE73160_SCLC_I,y=data), 
             color='red',
             size=4)+
  xlab("SCLC-I*")+ ylab("NE")+
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"),axis.title.x=element_text(size=60,face="bold"),
        axis.title.y=element_text(size=60,face="bold"),
        plot.title = element_text(size = 60, face = "bold"))+
  stat_cor(data=data,method="pearson",size=16,label.x=-13.5, label.y=0.8)
dev.off()

#GSE7097
#Correlation Scatter plot b/w SCLC-I* and NE

GSE7097_NE<-read.xlsx("GSE7097/GSE7097_NE_scores.xlsx",sheetIndex = 1)
GSE7097_SCLC_I<-read.xlsx("GSE7097/SCLC I GSE7097.xlsx",sheetIndex = 1)
GSE7097_SCLC_I<-GSE7097_SCLC_I$x
data<-cbind(GSE7097_NE,GSE7097_SCLC_I)
data<-as.data.frame(data)

jpeg(paste0("Correlation/","GSE7097 NE vs Inflamed.jpeg"),height = 1080,width = 1620)
ggplot(data,aes(x=GSE7097_SCLC_I ,y=NEScore)) + 
  geom_point(shape=18, colour = "blue",aes(size = 2),show.legend=FALSE)+
  xlab("SCLC-I*")+ ylab("NE")+
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"),axis.title.x=element_text(size=60,face="bold"),
        axis.title.y=element_text(size=60,face="bold"),
        plot.title = element_text(size = 60, face = "bold"))+
  stat_cor(data=data,method="pearson",size=16,label.x = -11.1,label.y = 0.53)
dev.off()

#GSE10841
#Correlation Scatter plot b/w SCLC-I* and NE

GSE10841_NE<-read.xlsx("GSE10841/GSE10841_NE_scores.xlsx",sheetIndex = 1)
GSE10841_SCLC_I<-read.xlsx("GSE10841/SCLC I GSE10841.xlsx",sheetIndex = 1)
GSE10841_SCLC_I<-GSE10841_SCLC_I$x
data<-cbind(GSE10841_NE,GSE10841_SCLC_I)
data<-as.data.frame(data)

jpeg(paste0("Correlation/","GSE10841 NE vs Inflamed.jpeg"),height = 1080,width = 1620)
ggplot(data,aes(x=GSE10841_SCLC_I ,y=NEScore)) + 
  geom_point(shape=18, colour = "blue",aes(size = 2),show.legend=FALSE)+
  xlab("SCLC-I*")+ ylab("NE")+
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"),axis.title.x=element_text(size=60,face="bold"),
        axis.title.y=element_text(size=60,face="bold"),
        plot.title = element_text(size = 60, face = "bold"))+
  stat_cor(data=data,method="pearson",size=16,label.x = -8.4,label.y = 0.53)
dev.off()

#GSE66294
#Correlation Scatter plot b/w SCLC-I* and NE

GSE66294_NE<-read.xlsx("GSE66294/GSE66294_NE_scores.xlsx",sheetIndex = 1)
GSE66294_SCLC_I<-read.xlsx("GSE66294/SCLC I GSE66294.xlsx",sheetIndex = 1)
GSE66294_SCLC_I<-GSE66294_SCLC_I$x
data<-cbind(GSE66294_NE,GSE66294_SCLC_I)
data<-as.data.frame(data)

jpeg(paste0("Correlation/","GSE66294 NE vs Inflamed.jpeg"),height = 1080,width = 1620)
ggplot(data,aes(x=GSE66294_SCLC_I ,y=NEScore)) + 
  geom_point(shape=18, colour = "blue",aes(size = 2),show.legend=FALSE)+
  xlab("SCLC-I*")+ ylab("NE")+
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"),axis.title.x=element_text(size=60,face="bold"),
        axis.title.y=element_text(size=60,face="bold"),
        plot.title = element_text(size = 60, face = "bold"))+
  stat_cor(data=data,method="pearson",size=16,label.x = -13.85,label.y = 0.78)
dev.off()

#GSE45120
#Correlation Scatter plot b/w SCLC-I* and NE

GSE45120_NE<-read.xlsx("GSE45120/GSE45120_NE_scores.xlsx",sheetIndex = 1)
GSE45120_SCLC_I<-read.xlsx("GSE45120/SCLC I GSE45120.xlsx",sheetIndex = 1)
GSE45120_SCLC_I<-GSE45120_SCLC_I$x
data<-cbind(GSE45120_NE,GSE45120_SCLC_I)
data<-as.data.frame(data)


jpeg(paste0("Correlation/","GSE45120 NE vs Inflamed.jpeg"),height = 1080,width = 1620)
ggplot(data,aes(x=GSE45120_SCLC_I ,y=NEScore)) + xlim(-16,-11)+
  geom_point(shape=18, colour = "blue",aes(size = 2),show.legend=FALSE)+
  xlab("SCLC-I*")+ ylab("NE")+
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"),axis.title.x=element_text(size=60,face="bold"),
        axis.title.y=element_text(size=60,face="bold"),
        plot.title = element_text(size = 60, face = "bold"))+
  stat_cor(data=data,method="pearson",size=16,label.x = -12.5,label.y = 0.72)
dev.off()

#GSE62609
#Correlation Scatter plot b/w SCLC-I* and NE

GSE62609_NE<-read.xlsx("GSE62609/GSE62609_NE_scores.xlsx",sheetIndex = 1)
GSE62609_SCLC_I<-read.xlsx("GSE62609/SCLC I GSE62609.xlsx",sheetIndex = 1)
GSE62609_SCLC_I<-GSE62609_SCLC_I$x
data<-cbind(GSE62609_NE,GSE62609_SCLC_I)
data<-as.data.frame(data)


jpeg(paste0("Correlation/","GSE62609 NE vs Inflamed.jpeg"),height = 1080,width = 1620)
ggplot(data,aes(x=GSE62609_SCLC_I ,y=NEScore)) + 
  geom_point(shape=18, colour = "blue",aes(size = 2),show.legend=FALSE)+
  xlab("SCLC-I*")+ ylab("NE")+
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"),axis.title.x=element_text(size=60,face="bold"),
        axis.title.y=element_text(size=60,face="bold"),
        plot.title = element_text(size = 60, face = "bold"))+
  stat_cor(data=data,method="pearson",size=16,label.x = -16.45,label.y = 0.74)
dev.off()

#GSE144457
#Correlation Scatter plot b/w SCLC-I* and NE

GSE144457_NE<-read.xlsx("GSE144457/GSE144457_NE_scores.xlsx",sheetIndex = 1)
GSE144457_SCLC_I<-read.xlsx("GSE144457/SCLC I GSE144457.xlsx",sheetIndex = 1)
GSE144457_SCLC_I<-GSE144457_SCLC_I$x
data<-cbind(GSE144457_NE,GSE144457_SCLC_I)
data<-as.data.frame(data)

jpeg(paste0("Correlation/","GSE144457 NE vs Inflamed.jpeg"),height = 1080,width = 1620)
ggplot(data,aes(x=GSE144457_SCLC_I ,y=NEScore)) + 
  geom_point(shape=18, colour = "blue",aes(size = 2),show.legend=FALSE)+
  xlab("SCLC-I*")+ ylab("NE")+
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=40,face="bold"),axis.title.x=element_text(size=60,face="bold"),
        axis.title.y=element_text(size=60,face="bold"),
        plot.title = element_text(size = 60, face = "bold"))+
  stat_cor(data=data,method="pearson",size=16,label.x = -14.7,label.y = 0.82)
dev.off()
