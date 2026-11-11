library(vegan)
library(gplots)
library(gclus)
library(tidyr)
install.packages("ade4")
install.packages("psych")
install.packages("fmsb")

path_fishdata <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/Catoctin_FishData.csv"
path_fishhabitat <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/Catoctin_FishHabitat.csv"
path_fishsites <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/Catoctin_FishSites.csv"
path_loggersites <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/Catoctin_LoggerSites.csv"
path_temperature <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/Catoctin_Temperature.csv"
path_cat <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/especes_categories.csv"

fish <- read.csv(path_fishdata)
fishhabitat <- read.csv(path_fishhabitat)
fishsites <- read.csv(path_fishsites)
loggersites <- read.csv(path_loggersites)
temperature <- read.csv(path_temperature)

#Partie prof

#wide_data <- fish %>%
#  pivot_wider(
#    names_from = c("ReachID","Species"),   # 比如年份、指标名
#   values_from = "Count"  # 比如数值、测量结果
#  )

#View(wide_data)
#wide<-reshape(fish, idvar="ReachID", timevar="Species", direction="wide")

#View(wide)

#fish2<-fish[,c("Species", "ReachID", "Count")]
#View(fish2)
#wide<-reshape(fish2, idvar="ReachID", timevar="Species", direction="wide")


#View(wide)
#View(fish2)
#View(wide)
#wide[is.na(wide)]<-0
#View(wide)
#wide[1,]
#wide[,1]

#rownames(wide)<-wide[,1]
#wide<-wide[,-1]
#View(wide)

#library(vegan)
#BC.fish<-vegdist(wide)
#BC.fish

#heatmap(as.matrix(BC.fish))

################################################################################
#1. Exploring dataset
################################################################################

library(vegan)
library(ade4)
library(pastecs) 
library(psych)
library(gplots)
library(fmsb)
library(permute)
library(dplyr)

#Work on the right directory
getwd()

#First, let's focus on the trout
#The line with only SATR and SAFO is preserved

fish_trout <- fish %>% 
  filter(Species %in% c("SATR", "SAFO"))

range(fish_trout$Length, na.rm = TRUE) #43 mm - 318 mm
range(fish_trout$Weight, na.rm = TRUE) #0.7 g - 316.5 g

mean(fish_trout$Length, na.rm = TRUE)       #95.42264 
median(fish_trout$Length, na.rm = TRUE)     #70
sd(fish_trout$Length, na.rm = TRUE)         # 49.18615
var(fish_trout$Length, na.rm = TRUE)        # 2419.278

#There is big variance 
#Let's plot the length for the 2 species
lengthSATR <- hist(fish_trout$Length[fish_trout$Species == "SATR"],probability = TRUE)
lengthSATR

lengthSAFO <- hist(fish_trout$Length[fish_trout$Species == "SAFO"],probability = TRUE)
lengthSAFO

mean(fish_trout$Weight, na.rm = TRUE)       # 17.69169
median(fish_trout$Weight, na.rm = TRUE)     #3.4
sd(fish_trout$Weight, na.rm = TRUE)         # 31.68259
var(fish_trout$Weight, na.rm = TRUE)      #1003.787

#Plot la dissimilarity matrix en fonction de la longueur pour SATR
##################
reachsatrlength <- fish_trout %>% 
  filter(Species == "SATR") %>% 
  group_by(ReachID) %>% 
  summarise(Length = mean(Length, na.rm = TRUE))

disSATR_num <- as.data.frame(reachsatrlength)
rownames(disSATR_num) <- disSATR_num$ReachID
disSATR_num$ReachID <- NULL 

SATR <- dist(disSATR_num, method = "euclidean")

heatmap(as.matrix(SATR), main = "Dissimilatity for length for SATR")

#Big distinction between the reaches for the length of the trout for satr. 


#Plot la dissimilarity matrix en fonction de la longueur pour SAFO
######################
reachsatrlength <- fish_trout %>% 
  filter(Species == "SAFO") %>% 
  group_by(ReachID) %>% 
  summarise(Length = mean(Length, na.rm = TRUE))

disSAFO_num <- as.data.frame(reachsatrlength)
rownames(disSAFO_num) <- disSAFO_num$ReachID
disSAFO_num$ReachID <- NULL 

SAFO <- dist(disSAFO_num, method = "euclidean")

heatmap(as.matrix(SAFO), main = "Dissimilatity for length for SAFO")
#

#Plot dissimilarity matrix for weight for SATR
###########################################################################
reachsatrweight <- fish_trout %>% 
  filter(Species == "SATR") %>% 
  group_by(ReachID) %>% 
  summarise(Weight = mean(Weight, na.rm = TRUE))

disSATR_numw <- as.data.frame(reachsatrweight)
rownames(disSATR_numw) <- disSATR_numw$ReachID
disSATR_numw$ReachID <- NULL 

SATRw <- dist(disSATR_numw, method = "euclidean")

heatmap(as.matrix(SATRw), main = "Dissimilatity for weight for SATR")

#Plot dissimilarity matrix for weight for SAFO
#################################################################################
reachsatrweight <- fish_trout %>% 
  filter(Species == "SAFO") %>% 
  group_by(ReachID) %>% 
  summarise(Weight = mean(Weight, na.rm = TRUE))

disSAFO_numw <- as.data.frame(reachsatrweight)
rownames(disSAFO_numw) <- disSAFO_numw$ReachID
disSAFO_numw$ReachID <- NULL 

SAFOw <- dist(disSAFO_numw, method = "euclidean")

heatmap(as.matrix(SAFOw), main = "Dissimilatity for weight for SAFO")


################################################################################
#Creating a map for each species weight
################################################################################


# --- 2. Paramètres (à adapter) ---
col_reach_fish  <- "ReachID"                # colonne ReachID dans le tableau de poissons
col_reach_sites <- names(fishsites)[1]      # colonne ReachID dans fishsites (à vérifier)
col_sp   <- "Species"
col_w    <- "Weight"
especes  <- c("SATR", "SAFO")               # les 2 codes d'espèces (à vérifier avec unique)

# --- 3. Nettoyage des types et des espaces ---
fish[[col_sp]]          <- trimws(fish[[col_sp]])
fish[[col_reach_fish]]  <- trimws(as.character(fish[[col_reach_fish]]))
fishsites[[col_reach_sites]] <- trimws(as.character(fishsites[[col_reach_sites]]))

# --- 4. Poids moyen par ReachID et par espèce ---
agg <- aggregate(fish[[col_w]],
                 by = list(reach = fish[[col_reach_fish]], sp = fish[[col_sp]]),
                 FUN = mean, na.rm = TRUE)  # sum pour le poids total
names(agg)[3] <- "poids"

# --- 5. Ajout des coordonnées ---
agg <- merge(agg, fishsites, by.x = "reach", by.y = col_reach_sites)

# --- 6. Deux graphiques côte à côte (version lisible) ---
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))     # marges réduites

max_poids <- max(agg$poids, na.rm = TRUE)     # échelle commune aux deux espèces
xlim <- extendrange(agg$Longitude, f = 0.05)  # mêmes limites d'axes pour les 2 graphiques
ylim <- extendrange(agg$Latitude,  f = 0.05)
taille <- function(p) 4 * sqrt(p / max_poids) # surface de la bulle proportionnelle au poids

for (e in especes) {
  d <- agg[agg$sp == e, ]
  d <- d[order(-d$poids), ]                   # grosses bulles dessinées d'abord, petites par-dessus
  
  plot(d$Longitude, d$Latitude,
       xlim = xlim, ylim = ylim,
       asp = 1 / cos(mean(agg$Latitude) * pi / 180),
       pch = 21, col = "white",
       bg = adjustcolor("brown", alpha.f = 0.5),  # bulles semi-transparentes
       cex = taille(d$poids),
       main = paste("Poids moyen -", e),
       xlab = "Longitude", ylab = "Latitude", las = 1)
  
  # étiquettes seulement pour les 3 ReachID les plus lourds
  top <- head(d, 3)
  text(top$Longitude, top$Latitude, top$reach, pos = 3, cex = .7)
  
  # légende de taille
  #leg <- round(c(0.25, 0.5, 1) * max_poids)
  #legend("bottomright", legend = paste(leg, "g"),
         #pch = 21, col = "white", pt.bg = adjustcolor("brown", alpha.f = 0.5),
         #pt.cex = taille(leg), title = "Poids moyen", bty = "n", y.intersp = 1.5)
  
}
  ################################################################################
  #Creating a map for each species Length
  ################################################################################

# --- 2. Paramètres (à adapter) ---
col_reach_fish  <- "ReachID"                # colonne ReachID dans le tableau de poissons
col_reach_sites <- names(fishsites)[1]      # colonne ReachID dans fishsites (à vérifier)
col_sp   <- "Species"
col_l    <- "Length"
especes  <- c("SATR", "SAFO")               # les 2 codes d'espèces (à vérifier avec unique)

# --- 3. Nettoyage des types et des espaces ---
fish[[col_sp]]          <- trimws(fish[[col_sp]])
fish[[col_reach_fish]]  <- trimws(as.character(fish[[col_reach_fish]]))
fishsites[[col_reach_sites]] <- trimws(as.character(fishsites[[col_reach_sites]]))

# --- 4. Poids moyen par ReachID et par espèce ---
agg <- aggregate(fish[[col_l]],
                 by = list(reach = fish[[col_reach_fish]], sp = fish[[col_sp]]),
                 FUN = mean, na.rm = TRUE)  # sum pour le poids total
names(agg)[3] <- "poids"

# --- 5. Ajout des coordonnées ---
agg <- merge(agg, fishsites, by.x = "reach", by.y = col_reach_sites)

# --- 6. Deux graphiques côte à côte (version lisible) ---
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))     # marges réduites

min_poids <- min(agg$poids, na.rm = TRUE)     # plus petite valeur, tous sites et espèces confondus

max_poids <- max(agg$poids, na.rm = TRUE)     # échelle commune aux deux espèces
xlim <- extendrange(agg$Longitude, f = 0.05)  # mêmes limites d'axes pour les 2 graphiques
ylim <- extendrange(agg$Latitude,  f = 0.05)
# taille entre 0.5 (valeur minimale) et 5 (valeur maximale)
taille <- function(p) 0.5 + 4.5 * (p - min_poids) / (max_poids - min_poids)

for (e in especes) {
  d <- agg[agg$sp == e, ]
  d <- d[order(-d$poids), ]                   # grosses bulles dessinées d'abord, petites par-dessus
  
  plot(d$Longitude, d$Latitude,
       xlim = xlim, ylim = ylim,
       asp = 1 / cos(mean(agg$Latitude) * pi / 180),
       pch = 21, col = "white",
       bg = adjustcolor("brown", alpha.f = 0.5),  # bulles semi-transparentes
       cex = taille(d$poids),
       main = paste("Length moyen -", e),
       xlab = "Longitude", ylab = "Latitude", las = 1)
  
  # étiquettes seulement pour les 3 ReachID les plus lourds
  top <- head(d, 3)
  text(top$Longitude, top$Latitude, top$reach, pos = 3, cex = .7)
  
  # légende de taille
  #leg <- round(c(0.25, 0.5, 1) * max_poids)
  #legend("bottomright", legend = paste(leg, "g"),
  #pch = 21, col = "white", pt.bg = adjustcolor("brown", alpha.f = 0.5),
  #pt.cex = taille(leg), title = "Poids moyen", bty = "n", y.intersp = 1.5)
  
}

#Creating a new dataset with average length and weight and categorie
##########################
############################################################
# TRUITES : fusion des donnees + analyse proies / predateurs
# Version PAR CLASSE DE TAILLE (petites / grandes truites) - V3
# A lancer APRES avoir charge `fish` (fish <- read.csv(path_fishdata))
# Les lignes changees dans cette version sont marquees "# V3"
############################################################

# install.packages(c("dplyr", "readxl", "writexl", "lmerTest"))   # une seule fois, AVANT les library()
library(dplyr)
library(readxl)
library(writexl)

############################################################
# 1. FUSION : fish + tableau des tailles et des poids par espece
############################################################

path_sp <- "C:/Users/soali/Desktop/MA1/Multivariate statistics in R/project/Rapport/fish_species_common_length_weight.xlsx"

# Lecture de l'onglet "Species table" : 13 especes (les lignes en dessous sont des notes)
sp <- read_excel(path_sp, sheet = "Species table", n_max = 13)

# On garde 10 colonnes, choisies par position (les titres ont des espaces et des parentheses)
sp <- sp[, c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11)]
names(sp) <- c("Species", "Nom_sci", "L_commune_cm", "L_max_cm", "L_ref_cm",
               "LWR_a", "LWR_b", "Poids_commun_g", "Categorie", "Type_alim")
sp$Species <- trimws(sp$Species)

# Nettoyage de fish (et suppression de l'ancienne colonne Categorie si elle existe)
fish$Species   <- trimws(fish$Species)
fish$ReachID   <- trimws(as.character(fish$ReachID))
fish$Categorie <- NULL

# Jointure : une ligne par mesure, avec les informations de son espece
fish_complet <- left_join(fish, sp, by = "Species")
fish_complet$L_ref_mm <- fish_complet$L_ref_cm * 10   # longueur de reference en mm (meme unite que Length)

sum(is.na(fish_complet$Categorie))   # doit donner 0 (sinon un code d'espece n'a pas ete reconnu)
head(fish_complet)

############################################################
# 2. PARAMETRES DE L'ANALYSE
############################################################

ratio <- 0.4                          # proie max = 40 % de la longueur du predateur (a justifier par la litterature)
trout_codes <- c("SATR", "SAFO")
L_truites <- fish_complet$Length[fish_complet$Species %in% trout_codes]          # V3 : toutes les longueurs de truites

# --- (3) Taille de la plus grande truite : quantile 95 % au lieu du max ---------- V3
sort(L_truites, decreasing = TRUE)[1:10]                                          # V3 : controle : 318 est-il isole ?
Lmax_truite <- as.numeric(quantile(L_truites, 0.95, na.rm = TRUE))                # V3 : robuste a un poisson exceptionnel
Lmax_truite                                                                       # V3

# --- (1) Limite entre petites et grandes truites ------------------------------------
# Calcul automatique : creux de la distribution des longueurs entre le 1er pic (0+) et le 2e pic (1+)
dens  <- density(L_truites, na.rm = TRUE)                                         # V3 : courbe lissee des longueurs
pente <- diff(sign(diff(dens$y)))                                                 # V3 : changements de pente
pics  <- which(pente == -2) + 1                                                   # V3 : sommets (maxima locaux)
creux <- which(pente ==  2) + 1                                                   # V3 : creux (minima locaux)
creux_1 <- creux[creux > pics[1]][1]                                              # V3 : 1er creux apres le 1er pic

if (length(pics) >= 2 && !is.na(creux_1)) {                                       # V3
  seuil_classe <- round(dens$x[creux_1])                                          # V3 : longueur (mm) du creux
} else {                                                                          # V3
  seuil_classe <- round(median(L_truites, na.rm = TRUE))                          # V3 : secours si un seul pic
  cat("Un seul pic trouve : seuil_classe = mediane des longueurs\n")              # V3
}                                                                                 # V3
seuil_classe                                                                      # V3 : controle

par(mfrow = c(1, 3))                                                              # V3 : graphiques de controle
hist(L_truites, breaks = 40, freq = FALSE,                                        # V3 : toutes truites + courbe
     main = "Toutes les truites", xlab = "Longueur (mm)")                         # V3
lines(dens, lwd = 2)                                                              # V3
abline(v = seuil_classe, col = "red", lwd = 2)                                    # V3
for (e in trout_codes) {                                                          # V3
  hist(fish_complet$Length[fish_complet$Species == e],                            # V3
       breaks = 40, main = e, xlab = "Longueur (mm)")                             # V3
  abline(v = seuil_classe, col = "red", lwd = 2)                                  # V3 : seuil actuel
}                                                                                 # V3
par(mfrow = c(1, 1))                                                              # V3

# --- (4) Taille utilisee pour classer une espece comme proie ------------------------ V3
# "ref" = taille de reference de l'Excel ; "obs" = mediane des longueurs mesurees dans tes donnees
taille_proie <- "ref"                                                             # V3 : mettre "obs" pour tester

L_obs <- fish_complet %>%                                                         # V3
  filter(!Species %in% trout_codes) %>%                                           # V3
  group_by(Species) %>%                                                           # V3
  summarise(L_mediane_obs = median(Length, na.rm = TRUE),                         # V3
            L_max_obs     = max(Length, na.rm = TRUE),                            # V3
            n_obs         = n(), .groups = "drop")                                # V3

comp_tailles <- sp %>%                                                            # V3
  select(Species, L_ref_cm) %>%                                                   # V3
  mutate(L_ref_mm = L_ref_cm * 10) %>%                                            # V3
  left_join(L_obs, by = "Species")                                                # V3
comp_tailles$L_proie_mm <- if (taille_proie == "obs") comp_tailles$L_mediane_obs else comp_tailles$L_ref_mm   # V3
comp_tailles                                                                      # V3 : controle ref vs observe

# Proies des GRANDES truites (>= seuil_classe) : especes que la plus grosse truite peut avaler
proies_codes <- comp_tailles$Species[which(!comp_tailles$Species %in% trout_codes &
                                             comp_tailles$L_proie_mm <= ratio * Lmax_truite)]   # V3
proies_codes    # controle

# Proies des PETITES truites (< seuil_classe) : especes qu'une truite de seuil_classe peut avaler
proies_pet_codes <- comp_tailles$Species[which(!comp_tailles$Species %in% trout_codes &
                                                 comp_tailles$L_proie_mm <= ratio * seuil_classe)]   # V3
proies_pet_codes    # controle (peut etre vide : la CPUE vaudra alors 0 partout)

# --- (2) Sensibilite au ratio : quelles especes entrent / sortent ? ----------------- V3
for (r_test in c(0.3, 0.4, 0.5)) {                                                # V3
  cat("\nratio =", r_test,                                                        # V3
      "\n  proies grandes truites :", comp_tailles$Species[which(!comp_tailles$Species %in% trout_codes &
                                                                   comp_tailles$L_proie_mm <= r_test * Lmax_truite)],   # V3
      "\n  proies petites truites :", comp_tailles$Species[which(!comp_tailles$Species %in% trout_codes &
                                                                   comp_tailles$L_proie_mm <= r_test * seuil_classe)], "\n")   # V3
}                                                                                 # V3

# Predateurs de truites (hors truites) : especes classees "Predator"
pred_codes <- sp$Species[sp$Categorie == "Predator" & !sp$Species %in% trout_codes]
pred_codes      # controle : MISA

# --- (5) Grande truite predatrice = peut manger une petite truite de taille mediane - V3
L_petites <- L_truites[L_truites < seuil_classe]                                  # V3
seuil_grande_truite <- median(L_petites, na.rm = TRUE) / ratio                    # V3 : remplace 100 / ratio
seuil_grande_truite                                                               # V3
sum(L_truites >= seuil_grande_truite, na.rm = TRUE)                               # V3 : nb de grandes truites predatrices

# --- (6) Nombre minimal de troncons pour faire un test ------------------------------ V3
min_troncons <- 5                                                                 # V3 : remplace 3

classes <- c("petite", "grande")

############################################################
# 3. PREDICTEURS PAR TRONCON (ReachID)
############################################################
# On travaille par troncon : l'abondance des proies ou des predateurs ne depend pas de la taille
# de chaque truite, donc pas de raisonnement circulaire. Dans une meme classe de taille,
# toutes les truites ont la meme liste de proies.

# Effort de peche de chaque troncon (secondes d'electropeche)
effort <- fish_complet %>%
  group_by(ReachID) %>%
  summarise(Effort = first(Effort), .groups = "drop")

# CPUE (captures pour 1000 s d'electropeche) d'un groupe d'especes, par troncon
cpue_especes <- function(codes) {
  fish_complet %>%
    filter(Species %in% codes) %>%
    group_by(ReachID) %>%
    summarise(n = sum(Count, na.rm = TRUE), .groups = "drop") %>%
    right_join(effort, by = "ReachID") %>%                          # garde aussi les troncons sans capture
    mutate(cpue = coalesce(as.numeric(n), 0) / Effort * 1000) %>%   # 0 si aucune capture
    select(ReachID, cpue)
}

# CPUE des grandes truites (>= seuil) d'une espece de truite, par troncon
cpue_grandes_truites <- function(code) {
  fish_complet %>%
    filter(Species == code, Length >= seuil_grande_truite) %>%
    group_by(ReachID) %>%
    summarise(n = n(), .groups = "drop") %>%
    right_join(effort, by = "ReachID") %>%
    mutate(cpue = coalesce(as.numeric(n), 0) / Effort * 1000) %>%
    select(ReachID, cpue)
}

predicteurs <- cpue_especes(proies_codes) %>% rename(proies_gr_cpue = cpue) %>%
  left_join(cpue_especes(proies_pet_codes) %>% rename(proies_pet_cpue = cpue), by = "ReachID") %>%
  left_join(cpue_especes(pred_codes) %>% rename(pred_autres_cpue = cpue), by = "ReachID") %>%
  left_join(cpue_grandes_truites("SATR") %>% rename(gt_SATR = cpue), by = "ReachID") %>%
  left_join(cpue_grandes_truites("SAFO") %>% rename(gt_SAFO = cpue), by = "ReachID")

# Table des truites (une ligne par poisson) avec les predicteurs de leur troncon
# Predateurs d'une truite = autres predateurs (MISA) + grandes truites de L'AUTRE espece
truites <- fish_complet %>%
  filter(Species %in% trout_codes, !is.na(Length)) %>%
  left_join(predicteurs, by = "ReachID") %>%
  mutate(Classe      = ifelse(Length < seuil_classe, "petite", "grande"),
         proies_cpue = ifelse(Classe == "petite", proies_pet_cpue, proies_gr_cpue),
         pred_cpue   = pred_autres_cpue + ifelse(Species == "SAFO", gt_SATR, gt_SAFO))

table(truites$Species, truites$Classe)   # controle du nombre de truites par classe

# Table par troncon, par espece de truite et par classe (une ligne = un troncon)
reach_tab <- truites %>%
  group_by(ReachID, Species, Classe) %>%
  summarise(longueur = mean(Length),
            poids = mean(Weight, na.rm = TRUE),
            n = n(),
            proies_cpue = first(proies_cpue),
            pred_cpue = first(pred_cpue),
            .groups = "drop")

table(reach_tab$Species, reach_tab$Classe)   # V3 : nombre de troncons par groupe (a comparer a min_troncons)

# Controle : les predicteurs doivent varier d'un troncon a l'autre
summary(reach_tab[, c("proies_cpue", "pred_cpue")])

############################################################
# 4. CORRELATIONS DE SPEARMAN (une valeur par troncon, dans chaque classe)
############################################################

res_spearman <- data.frame()                                                      # V3 : tableau des resultats

for (e in trout_codes) {
  for (cl in classes) {
    d <- reach_tab[reach_tab$Species == e & reach_tab$Classe == cl, ]
    cat("\n=====", e, "-", cl, "( nombre de troncons =", nrow(d), ") =====\n")
    for (v in c("proies_cpue", "pred_cpue")) {
      if (nrow(d) >= min_troncons && length(unique(d[[v]])) > 1) {                # V3 : min_troncons
        r <- cor.test(d$longueur, d[[v]], method = "spearman", exact = FALSE)
        cat("Longueur ~", v, ": rho =", round(r$estimate, 2), "  p =", round(r$p.value, 3), "\n")
        res_spearman <- rbind(res_spearman,                                       # V3 : on garde le resultat
                              data.frame(Espece = e, Classe = cl, Predicteur = v, # V3
                                         n_troncons = nrow(d),                    # V3
                                         rho = unname(r$estimate), p = r$p.value))   # V3
      } else {
        cat("Longueur ~", v, ": trop peu de troncons ou predicteur constant, test impossible\n")
      }
    }
  }
}

res_spearman$p_holm <- p.adjust(res_spearman$p, method = "holm")                  # V3 : correction tests multiples
print(res_spearman)                                                               # V3

############################################################
# 5. MODELES MIXTES (une ligne = un poisson, troncon en effet aleatoire, dans chaque classe)
############################################################
# Les poissons d'un meme troncon ne sont pas independants : (1 | ReachID) en tient compte.
# Les predicteurs sont centres-reduits (scale) : le coefficient = effet d'1 ecart-type.
# Reponse en log : un coefficient b correspond a une variation d'environ 100 * b %.

library(lmerTest)   # charge lme4 et ajoute les p-values

res_mixtes <- data.frame()                                                        # V3 : tableau des resultats

for (e in trout_codes) {
  for (cl in classes) {
    for (resp in c("Length", "Weight")) {
      d <- truites[truites$Species == e & truites$Classe == cl & !is.na(truites[[resp]]), ]
      d$proies_z <- as.numeric(scale(d$proies_cpue))
      d$pred_z   <- as.numeric(scale(d$pred_cpue))
      
      # On garde seulement les predicteurs qui varient (un predicteur constant fait echouer le modele)
      vars <- c("proies_z", "pred_z")
      vars <- vars[sapply(vars, function(v) !any(is.na(d[[v]])) && length(unique(d[[v]])) > 1)]
      nb_troncons <- length(unique(d$ReachID))
      cat("\n=====", e, "-", cl, "-", resp, "- predicteurs utilises :", vars,
          "- troncons :", nb_troncons, "=====\n")
      if (length(vars) == 0 || nb_troncons < min_troncons || nb_troncons >= nrow(d)) next   # V3 : min_troncons
      
      f <- as.formula(paste0("log(", resp, ") ~ ", paste(vars, collapse = " + "), " + (1 | ReachID)"))
      m <- lmer(f, data = d)
      print(summary(m))
      
      cf <- coef(summary(m))                                                      # V3 : tableau des coefficients
      cf <- cf[rownames(cf) != "(Intercept)", , drop = FALSE]                     # V3 : on retire l'intercept
      res_mixtes <- rbind(res_mixtes,                                             # V3
                          data.frame(Espece = e, Classe = cl, Reponse = resp,     # V3
                                     Predicteur = rownames(cf),                   # V3
                                     n_poissons = nrow(d), n_troncons = nb_troncons,   # V3
                                     Estimate = cf[, "Estimate"],                 # V3
                                     p = cf[, "Pr(>|t|)"], row.names = NULL))     # V3
    }
  }
}

res_mixtes$p_holm <- p.adjust(res_mixtes$p, method = "holm")                      # V3 : correction tests multiples
print(res_mixtes)                                                                 # V3

############################################################
# 6. FIGURE : longueur moyenne par troncon vs proies / predateurs, par classe
############################################################

lab <- c(proies_cpue = "Proies potentielles (captures / 1000 s)",
         pred_cpue   = "Predateurs de truites (captures / 1000 s)")

png("truites_proies_predateurs_classes.png", width = 2000, height = 3000, res = 200)
par(mfrow = c(4, 2), mar = c(4.5, 4.5, 3, 1))
for (e in trout_codes) {
  for (cl in classes) {
    d <- reach_tab[reach_tab$Species == e & reach_tab$Classe == cl, ]
    for (v in c("proies_cpue", "pred_cpue")) {
      if (nrow(d) == 0) { plot.new(); next }                                         # case vide
      plot(d[[v]], d$longueur,
           pch = 21, col = "white", bg = adjustcolor("brown", alpha.f = 0.6),
           cex = 0.8 + 2 * sqrt(d$n / max(d$n)),      # taille du point = nombre de truites mesurees
           xlab = lab[v], ylab = "Longueur moyenne (mm)",
           main = paste(e, "-", cl, "-", lab[v]), cex.main = 0.8, las = 1)
    }
  }
}
dev.off()

############################################################
# 7. EXPORT : un fichier Excel avec 6 onglets (fermer le fichier dans Excel avant !)
############################################################

write_xlsx(list(fish_complet = fish_complet,
                truites_predicteurs = truites,
                troncons = reach_tab,
                spearman = res_spearman,                                          # V3
                modeles_mixtes = res_mixtes,                                      # V3
                tailles_proies = comp_tailles),                                   # V3
           "fish_complet_classes.xlsx")                                        # MODIFIE