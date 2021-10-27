#Fig 3C, 3D
 
library(xlsx)
library(ggplot2)
library(ggpubr)

#Fig 3C
data<-read.xlsx("Fig3C data.xlsx",sheetIndex = 1)
row.names(data)<-data[,1]
data[,1]<-NULL
jpeg(paste0("Plots/","Fig3C.jpeg"),height = 1080,width = 1620)
ggplot(data,aes(x=KS.vs.ssGSEA..Hallmark.EMT.,y=X76GS.vs.ssGSEA..Hallmark.EMT.)) + xlim(0.5,1)+ylim(-1,-0.5)+
  geom_point(shape=18, colour = "blue", aes(size = 2),show.legend=FALSE)+
  xlab("ssGSEA (EMT) vs KS")+ ylab("76GS vs ssGSEA (EMT)")+
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=14,face="bold"),axis.title.x=element_text(size=50,face="bold"),
        axis.title.y=element_text(size=50,face="bold"))+
  stat_cor(method="pearson",size=16,label.x=0.87, label.y=-0.52)
dev.off()

#Fig 3D
data<-read.xlsx("Fig3D data.xlsx",sheetIndex = 1)
row.names(data)<-data[,1]
data[,1]<-NULL
jpeg(paste0("Plots/","Fig3D.jpeg"),height = 1080,width = 1620)
ggplot(data,aes(x=NE.vs.KS,y=X76GS.vs.NE)) + xlim(-1,-0.25)+ylim(0.4,1)+
  geom_point(shape=18, colour = "blue", aes(size = 2),show.legend=FALSE)+
  xlab("NE vs KS")+ ylab("76GS vs NE")+
  theme(axis.text=element_text(size=40),
        axis.title=element_text(size=14,face="bold"),axis.title.x=element_text(size=60,face="bold"),
        axis.title.y=element_text(size=60,face="bold"))+
  stat_cor(method="pearson",size=16,label.x=-0.45, label.y=0.975)
  dev.off()
