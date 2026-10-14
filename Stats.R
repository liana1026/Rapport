library(vegan)
library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------------------------------------
#Heatmap

path_fishdata <- "C:/Users/nbo15/Documents/EPFL/Master/EPFL_Semestre_1_documents/MSR/Project/Catoctin_FishData.csv"
fishdata <- read.csv(path_fishdata)

# Count rempli          -> on garde le total
# Count vide + Length   -> 1 poisson mesuré (truites SATR / SAFO)
# Count vide, Length vide (2 lignes de FOM27) -> 0
fishdata_modified <- fishdata %>%
  mutate(Count = case_when(!is.na(Count)  ~ as.numeric(Count),
                           !is.na(Length) ~ 1,
                           TRUE           ~ 0))

# Matrice reach x espèce
wide <- fishdata_modified %>% 
  select(ReachID, Species, Count) %>% 
  pivot_wider(names_from = Species, values_from = Count, values_fn = sum, values_fill = 0) %>% 
  as.data.frame()
rownames(wide) <- wide$ReachID
wide <- wide %>% select(-ReachID)

BC.fishdata <- vegdist(wide, method = "bray")
heatmap(as.matrix(BC.fishdata))

#----------------------------------------------------------------------------------------------------
# Nettoyage données

path_fishhabitat <- "C:/Users/nbo15/Documents/EPFL/Master/EPFL_Semestre_1_documents/MSR/Project/Catoctin_FishHabitat.csv"

habitat <- read.csv(path_fishhabitat)
habitat <- habitat[, names(habitat) != ""]          # 2 dernières colonnes vides

habitat <- habitat %>%
  mutate(across(where(is.character), ~ toupper(trimws(.x)))) %>%
  mutate(across(starts_with("Mesohabitat"), ~ recode(.x, "R5U" = "RUN")),
         across(starts_with("Substrate"), ~ recode(.x, "MO" = "MOSS", "S" = NA_character_, "RO" = NA_character_)),
         DepthL = suppressWarnings(as.numeric(DepthL)))

#--------------------------------------------------------------------------------------------------------
# Largeur moyenne et coefficient de variation (SD / moyenne) par reach

width <- habitat %>%
  mutate(WettedWidth = suppressWarnings(as.numeric(WettedWidth))) %>%
  group_by(ReachID) %>%
  summarise(width_mean = mean(WettedWidth, na.rm = TRUE),
            width_sd   = sd(WettedWidth,   na.rm = TRUE),
            width_cv   = width_sd / width_mean,
            .groups = "drop")

print(width)

#--------------------------------------------------------------------------------------------------------
# % de pools / riffles / runs par reach (3 observations L, M, R par transect)

meso <- habitat %>%
  select(ReachID, MesohabitatL, MesohabitatM, MesohabitatR) %>%
  pivot_longer(-ReachID, values_to = "meso") %>%
  filter(!is.na(meso)) %>%
  count(ReachID, meso) %>%
  group_by(ReachID) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = meso, values_from = pct,
              names_prefix = "pct_", values_fill = 0)

print(meso)

#--------------------------------------------------------------------------------------------------------
# % de chaque substrat par reach
# Top + Bottom mélangés. 
# Pour surface seul --> select(ReachID, starts_with("SubstrateTop"))
substrate <- habitat %>%
  select(ReachID, starts_with("SubstrateTop"), starts_with("SubstrateBottom")) %>%
  pivot_longer(-ReachID, values_to = "substrate") %>%
  filter(!is.na(substrate)) %>%
  count(ReachID, substrate) %>%
  group_by(ReachID) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = substrate, values_from = pct,
              names_prefix = "sub_", values_fill = 0)

print(substrate)

#---------------------------------------------------------------------------------------------------------
# Fusion des env data avec les fiish --> FULL DATA COMPLETED

env <- width %>%
  left_join(meso, by = "ReachID") %>%
  left_join(substrate, by = "ReachID")

full_data <- wide %>%
  tibble::rownames_to_column("ReachID") %>%
  left_join(env, by = "ReachID")
