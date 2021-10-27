# Overlap Analysis
library(xlsx)
Cancer_hallmark<-list()
for (i in 1:5)
{
  Cancer_hallmark[i]<-read.xlsx("Overlap Analysis/Cancer Hallmarks gene clusters.xlsx",sheetIndex = i)
}

CCLE<-list()
for (i in 1:5)
{
  CCLE[i]<-read.xlsx("Overlap Analysis/CCLE gene clusters.xlsx",sheetIndex = i)
}
intersect<-matrix(nrow=5,ncol = 5)
for (i in 1:5)
{
  for (j in 1:5)
  {
    intersect[i,j]<-length(intersect(Cancer_hallmark[[i]],CCLE[[j]]))/length(Cancer_hallmark[[i]])
  }
}
write.xlsx(intersect,"Overlap Analysis/Overlapped files/CCLE_ref_Cancer_hallmark.xlsx")

GSE73160<-list()
for (i in 1:5)
{
  GSE73160[i]<-read.xlsx("Overlap Analysis/GSE73160 gene clusters.xlsx",sheetIndex = i)
}

intersect<-matrix(nrow=5,ncol = 5)
for (i in 1:5)
{
  for (j in 1:5)
  {
    intersect[i,j]<-length(intersect(Cancer_hallmark[[i]],GSE73160[[j]]))/length(Cancer_hallmark[[i]])
  }
}
write.xlsx(intersect,"Overlap Analysis/Overlapped files/GSE73160_ref_Cancer_hallmark.xlsx")

UTSW<-list()
for (i in 1:5)
{
  UTSW[i]<-read.xlsx("Overlap Analysis/SCLC UTSW gene clusters.xlsx",sheetIndex = i)
}

intersect<-matrix(nrow=5,ncol = 5)

for (i in 1:5)
{
  for (j in 1:5)
  {
    intersect[i,j]<-length(intersect(Cancer_hallmark[[i]],UTSW[[j]]))/length(Cancer_hallmark[[i]])
  }
}
write.xlsx(intersect,"Overlap Analysis/Overlapped files/UTSW_ref_Cancer_hallmark.xlsx")


Stewart<-list()
for (i in 1:5)
{
  Stewart[i]<-read.xlsx("Overlap Analysis/Stewart_gene_clusters.xlsx",sheetIndex = i)
}

intersect<-matrix(nrow=5,ncol = 5)

for (i in 1:5)
{
  for (j in 1:5)
  {
    intersect[i,j]<-length(intersect(Cancer_hallmark[[i]],Stewart[[j]]))/length(Cancer_hallmark[[i]])
  }
}
write.xlsx(intersect,"Overlap Analysis/Overlapped files/Stewart_ref_Cancer_hallmark.xlsx")