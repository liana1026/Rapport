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
# A lancer APRES avoir charge `fish` (fish <- read.csv(path_fishdata))
############################################################

library(dplyr)
library(readxl)
library(writexl)
install.packages(c("readxl", "writexl", "lmerTest"))   # si besoin

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

ratio <- 0.4                          # proie max = 40 % de la longueur du predateur (a justifier, tester 0.3 et 0.5)
trout_codes <- c("SATR", "SAFO")
Lmax_truite <- max(fish_complet$Length[fish_complet$Species %in% trout_codes], na.rm = TRUE)   # 318 mm

# Proies potentielles : especes (hors truites) que la plus grosse truite observee peut avaler
proies_codes <- sp$Species[!sp$Species %in% trout_codes & sp$L_ref_cm * 10 <= ratio * Lmax_truite]
proies_codes    # controle : ETFL RHAT COCA CLFU COGI RHCA

# Predateurs de truites (hors truites) : especes classees "Predator"
pred_codes <- sp$Species[sp$Categorie == "Predator" & !sp$Species %in% trout_codes]
pred_codes      # controle : MISA

# Une truite est consideree "grande" si elle peut manger une truite de 100 mm (100 / 0.4 = 250 mm)
seuil_grande_truite <- 100 / ratio

############################################################
# 3. PREDICTEURS PAR TRONCON (ReachID)
############################################################
# On travaille par troncon : l'abondance des proies ou des predateurs ne depend pas de la taille
# de chaque truite, donc pas de raisonnement circulaire (une grosse truite aurait sinon
# mecaniquement plus de proies "mangeables").

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

predicteurs <- cpue_especes(proies_codes) %>% rename(proies_cpue = cpue) %>%
  left_join(cpue_especes(pred_codes) %>% rename(pred_autres_cpue = cpue), by = "ReachID") %>%
  left_join(cpue_grandes_truites("SATR") %>% rename(gt_SATR = cpue), by = "ReachID") %>%
  left_join(cpue_grandes_truites("SAFO") %>% rename(gt_SAFO = cpue), by = "ReachID")

# Table des truites (une ligne par poisson) avec les predicteurs de leur troncon
# Predateurs d'une truite = autres predateurs (MISA) + grandes truites de L'AUTRE espece
truites <- fish_complet %>%
  filter(Species %in% trout_codes, !is.na(Length)) %>%
  left_join(predicteurs, by = "ReachID") %>%
  mutate(pred_cpue = pred_autres_cpue + ifelse(Species == "SAFO", gt_SATR, gt_SAFO))

# Table par troncon et par espece de truite (une ligne = un troncon)
reach_tab <- truites %>%
  group_by(ReachID, Species) %>%
  summarise(longueur = mean(Length),
            poids = mean(Weight, na.rm = TRUE),
            n = n(),
            proies_cpue = first(proies_cpue),
            pred_cpue = first(pred_cpue),
            .groups = "drop")

# Controle : les predicteurs doivent varier d'un troncon a l'autre
summary(reach_tab[, c("proies_cpue", "pred_cpue")])

############################################################
# 4. CORRELATIONS DE SPEARMAN (une valeur par troncon)
############################################################

for (e in trout_codes) {
  d <- reach_tab[reach_tab$Species == e, ]
  cat("\n=====", e, "( nombre de troncons =", nrow(d), ") =====\n")
  for (v in c("proies_cpue", "pred_cpue")) {
    if (length(unique(d[[v]])) > 1) {
      r <- cor.test(d$longueur, d[[v]], method = "spearman", exact = FALSE)
      cat("Longueur ~", v, ": rho =", round(r$estimate, 2), "  p =", round(r$p.value, 3), "\n")
    } else {
      cat("Longueur ~", v, ": predicteur constant, test impossible\n")
    }
  }
}

############################################################
# 5. MODELES MIXTES (une ligne = un poisson, troncon en effet aleatoire)
############################################################
# Les poissons d'un meme troncon ne sont pas independants : (1 | ReachID) en tient compte.
# Les predicteurs sont centres-reduits (scale) : le coefficient = effet d'1 ecart-type.
# Reponse en log : un coefficient b correspond a une variation d'environ 100 * b %.

library(lmerTest)   # charge lme4 et ajoute les p-values

for (e in trout_codes) {
  for (resp in c("Length", "Weight")) {
    d <- truites[truites$Species == e & !is.na(truites[[resp]]), ]
    d$proies_z <- as.numeric(scale(d$proies_cpue))
    d$pred_z   <- as.numeric(scale(d$pred_cpue))
    
    # On garde seulement les predicteurs qui varient (un predicteur constant fait echouer le modele)
    vars <- c("proies_z", "pred_z")
    vars <- vars[sapply(vars, function(v) !any(is.na(d[[v]])) && length(unique(d[[v]])) > 1)]
    cat("\n=====", e, "-", resp, "- predicteurs utilises :", vars, "=====\n")
    if (length(vars) == 0) next
    
    f <- as.formula(paste0("log(", resp, ") ~ ", paste(vars, collapse = " + "), " + (1 | ReachID)"))
    m <- lmer(f, data = d)
    print(summary(m))
  }
}

############################################################
# 6. FIGURE : longueur moyenne par troncon vs proies / predateurs
############################################################

lab <- c(proies_cpue = "Proies potentielles (captures / 1000 s)",
         pred_cpue   = "Predateurs de truites (captures / 1000 s)")

png("truites_proies_predateurs.png", width = 2000, height = 1600, res = 200)
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))
for (e in trout_codes) {
  d <- reach_tab[reach_tab$Species == e, ]
  for (v in c("proies_cpue", "pred_cpue")) {
    plot(d[[v]], d$longueur,
         pch = 21, col = "white", bg = adjustcolor("brown", alpha.f = 0.6),
         cex = 0.8 + 2 * sqrt(d$n / max(d$n)),      # taille du point = nombre de truites mesurees
         xlab = lab[v], ylab = "Longueur moyenne (mm)",
         main = paste(e, "-", lab[v]), cex.main = 0.8, las = 1)
  }
}
dev.off()

############################################################
# 7. EXPORT : un fichier Excel avec 3 onglets
############################################################

write_xlsx(list(fish_complet = fish_complet,
                truites_predicteurs = truites,
                troncons = reach_tab),
           "fish_complet.xlsx")