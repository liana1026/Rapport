library(vegan)
library(gplots)
library(gclus)
library(tidyr)

path_fishdata <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/Catoctin_FishData.csv"
path_fishhabitat <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/Catoctin_FishHabitat.csv"
path_fishsites <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/Catoctin_FishSites.csv"
path_loggersites <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/Catoctin_LoggerSites.csv"
path_temperature <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/Catoctin_Temperature.csv"

fish <- read.csv(path_fishdata)
fishhabitat <- read.csv(path_fishhabitat)
fishsites <- read.csv(path_fishsites)
loggersites <- read.csv(path_loggersites)
temperature <- read.csv(path_temperature)

#Partie prof

wide_data <- fish %>%
  pivot_wider(
    names_from = c("ReachID","Species"),   # 比如年份、指标名
    values_from = "Count"  # 比如数值、测量结果
  )

View(wide_data)
wide<-reshape(fish, idvar="ReachID", timevar="Species", direction="wide")

View(wide)

fish2<-fish[,c("Species", "ReachID", "Count")]
View(fish2)
wide<-reshape(fish2, idvar="ReachID", timevar="Species", direction="wide")


View(wide)
View(fish2)
View(wide)
wide[is.na(wide)]<-0
View(wide)
wide[1,]
wide[,1]

rownames(wide)<-wide[,1]
wide<-wide[,-1]
View(wide)

library(vegan)
BC.fish<-vegdist(wide)
BC.fish

heatmap(as.matrix(BC.fish))
