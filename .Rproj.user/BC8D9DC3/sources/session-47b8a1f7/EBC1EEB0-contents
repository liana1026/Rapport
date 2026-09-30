library(tidyr)

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
