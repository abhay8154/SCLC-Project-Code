#Calculation of NE Scores for RNA-seq and microarray SCLC datasets

library(xlsx)

#Subset NE & Non NE expression values from datasets
ds<-c("CCLE","SCLC_GSE73160")
p<-list()

for ( i in ds){
  k = which(ds==i)
  df <- read.csv(paste0("Datasets/", i,".csv"), row.names = 1)
  NE<-read.xlsx("Datasets/NE.xlsx",sheetIndex = i,header = FALSE)
  NE<-NE$X1
  NonNE<-read.xlsx("Datasets/NonNE.xlsx",sheetIndex = k,header = FALSE)
  NonNE<-NonNE$X1
  NE_df<-df[c(NE,NonNE),]
  NE_diff<-read.xlsx("Datasets/NE_diff.xlsx",sheetIndex = i)
  Non_NE_diff<-read.xlsx("Datasets/Non_NE_diff.xlsx",sheetIndex = i)
  
  # Calculate Pearson Correlations & NE scores
  
  correl_NE<-cor(x=NE_df,y=NE_diff,method = "pearson")
  correl_NonNE<-cor(x=NE_df,y=Non_NE_diff,method = "pearson")
  NE_score<-(correl_NE-correl_NonNE)/2
  p[[k]] <- NE_score
  write.xlsx(p[[k]],"NE_scores.xlsx",sheetName = i,append = TRUE)
}