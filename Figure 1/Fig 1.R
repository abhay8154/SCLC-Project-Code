#1A i-iv & 1B i-iv

#df = dataframe  for sequencing data, genes in rows, samples in columns 
#df = CCLE for A i-iv & df=GSE73160 for B i-iv
#genes = gene sets, either of GS_50, GS_105, GS_50+105 or GS_33+50+105 
#sheet = select sheet containing specific gene set in case of multiple gene sets

library(corrplot)
library(ggcorrplot)
library(readxl)

cols <- colorRampPalette(c('blue', 'white', 'red'))
df<-read.csv("Datasets/GSE73160.csv",row.names = 1)

#uncomment below code line to read CCLE dataset instead
#df<-read.csv("Datasets/CCLE.csv",row.names = 1) 

  jpeg(paste0("Plots/","GSE73160 HC.jpeg"),height = 1080,width = 1620)
  genes<-as.data.frame(read_excel("Datasets/GS_50 & GS_105.xlsx",sheet=1))
    df_sub <- df[genes[,2],]
    col <- genes[complete.cases(df_sub),3]
    df_sub <- df_sub[complete.cases(df_sub),]
    corr <- cor(t(df_sub) , method = "spearman")  #Correlation matrix
    corr.p <- cor_pmat(t(df_sub), method = "spearman" )
    ord <- corrMatOrder(corr, "hclust")
    col <- col[ord]
       corrplot(corr,method = "color",
             hc.order = FALSE,
             type = "lower",
             outline.color = "white", p.mat = as.matrix(corr.p), sig.level = 0.05, insig = "pch",
             show.diag = TRUE, show.legend = FALSE, tl.col = col, tl.cex = 1.5, pch.cex = 2, 
             order = "hclust",cl.cex=3,
             col = cols(256), title =paste0("GSE73160 with ",length(genes$Genes)," genes HC"),mar=c(0,0,1,0))
   
  dev.off()
