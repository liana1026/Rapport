cwd <- getwd()
data <- read.csv("Catoctin_FishData.csv")

spenr <- length(unique(data$Species))
