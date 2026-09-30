#----0. Packages----
install.packages(c("vegan", "pastecs", "psych", "gplots", "cluster"))
library(vegan) #vegdist(), decostand()
library(pastecs) #stat.desc()
library(psych) #describeBy
library(gplots) #heatmap.2()
library(cluster) # daisy()

#----Helper from notes----
image.real <- function(mat, main = "") {
  mat <- as.matrix(mat)
  n_r <- nrow(mat); n_c <- ncol(mat)
  image(t(mat)[, n_r:1], axes = FALSE,
        col = hcl.colors(15, palette = "viridis"), main = main)
  axis(1, at = seq(0, 1, length = n_c), labels = colnames(mat), las = 2, cex.axis = 0.7)
  axis(2, at = seq(0, 1, length = n_r), labels = rev(rownames(mat)), las = 1, cex.axis = 0.7)
}

# ---- 1. Load the data ----
fish <- read.csv("Catoctin_FishData.csv")
habitat <- read.csv("Catoctin_FishHabitat.csv")
sites <- read.csv("Catoctin_FishSites.csv")
temp <- read.csv("Catoctin_Temperature.csv")

#---- 2. Looking first ----
