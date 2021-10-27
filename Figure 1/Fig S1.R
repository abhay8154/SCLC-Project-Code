#S1

library(readxl)
library(VennDiagram)
library(tidyverse)
library(hrbrthemes)
library(tm)
library(proustr)

#GS_33
gene_set_33<-read_excel("Datasets/GS_33.xlsx", sheet = 1)
gene_set_33<-gene_set_33$Genes
#GS_50
gene_set_50<-read_excel("Datasets/GS_50 & GS_105.xlsx", sheet = 1)
gene_set_50<-gene_set_50$Genes
#GS_105
gene_set_105<-read_excel("Datasets/GS_50 & GS_105.xlsx", sheet= 2)
gene_set_105<-gene_set_105$Genes

library(ggVennDiagram)
ggVennDiagram(x=list(gene_set_33,gene_set_50,gene_set_105), label_alpha = 0.5,
              category.names = c("Udyavar et al., 2017 (33)","Zhang et al., 2018 (50)",
                                 "Groves et al., 2021 (105)"),
              show_intersect = TRUE)
