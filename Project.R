library(vegan)
library(gplots)
library(gclus)
library(pastecs)

path_fishdata <- "C:/Users/nbo15/Documents/EPFL/Master/EPFL_Semestre_1_documents/MSR/Project/Catoctin_FishData.csv"
path_fishhabitat <- "C:/Users/nbo15/Documents/EPFL/Master/EPFL_Semestre_1_documents/MSR/Project/Catoctin_FishHabitat.csv"
path_fishsites <- "C:/Users/nbo15/Documents/EPFL/Master/EPFL_Semestre_1_documents/MSR/Project/Catoctin_FishSites.csv"
path_loggersites <- "C:/Users/nbo15/Documents/EPFL/Master/EPFL_Semestre_1_documents/MSR/Project/Catoctin_LoggerSites.csv"
path_temperature <- "C:/Users/nbo15/Documents/EPFL/Master/EPFL_Semestre_1_documents/MSR/Project/Catoctin_Temperature.csv"

fishdata <- read.csv(path_fishdata)
fishhabitat <- read.csv(path_fishhabitat)
fishsites <- read.csv(path_fishsites)
loggersites <- read.csv(path_loggersites)
temperature <- read.csv(path_temperature)


stat.desc(fishhabitat)


