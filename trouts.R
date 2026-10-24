library(vegan)
library(pastecs) 
library(psych)
library(gplots)
library (ggplot2)
library(tidyr)


fish <- read.csv("Catoctin_FishData.csv") #assumes this script in same folder as the data files
loc <- read.csv("Catoctin_FishSites.csv") #assumes this script in same folder as the data files

trouts <- fish[fish$Species %in% c("SAFO", "SATR"), ]

trouts$Count <- NULL

# Salvelinus fontinalis aka. Brook trout/sea trout / aurora trout
SATR <- trouts[trouts$Species == "SATR",]
SATR_OW <- SATR[SATR$Watershed == "Owens",]
SATR_BH <- SATR[SATR$Watershed == "Big Hunting",]

#salmo trutta aka. brown trout
SAFO <- trouts[trouts$Species == "SAFO",]
SAFO_OW <- SAFO[SAFO$Watershed == "Owens",]
SAFO_BH <- SAFO[SAFO$Watershed == "Big Hunting",]

#Group Reaches (FBB, FBM, FOA, FOB, FOC, FOD, FOM)
trouts$ReachGroup <- sub("[0-9]+$", "", trouts$Reach)

# first overview over the Species average Length [mm] & size [g]

describeBy(trouts[, c("Length", "Weight")], group = trouts$Species)
describeBy(trouts[, c("Length", "Weight")],group = interaction(trouts$Species, trouts$Watershed))

# compare different ReachID - labels - without number there are 7

describeBy(trouts[, c("Length", "Weight")], group = trouts$ReachGroup)


# histogram
trouts_hist <- pivot_longer(
  trouts,
  cols = c(Length, Weight),
  names_to = "Variable",
  values_to = "Value"
)

ggplot(trouts_hist, aes(x = Value, fill = Watershed)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20) +
  facet_grid(Variable ~ Species, scales = "free_x") +
  labs(
    title = "Trout Length [mm] and Weight [g] Distributions",
    x = NULL,
    y = "Count"
  )
ggplot(trouts_hist,
       aes(x = log10(Value), fill = Watershed)) +
  geom_histogram(position = "identity",
                 alpha = 0.5,
                 bins = 20) +
  facet_grid(Variable ~ Species, scales = "free_x") +
  labs(
    title = "Log10-transformed Trout Length and Weight Distributions",
    x = "log10(Value)",
    y = "Count"
  )

ggplot(trouts_hist,
       aes(x = Value, fill = Watershed)) +
  geom_histogram(position = "identity",
                 alpha = 0.5,
                 bins = 20) +
  scale_x_log10() +
  facet_grid(Variable ~ Species, scales = "free_x") +
  labs(
    title = "Trout Length and Weight Distributions",
    x = NULL,
    y = "Count"
  )

# length & size relation) 
ggplot(trouts,
       aes(x = Length, y = Weight, color = Watershed)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~ Species) +
  labs(
    title = "Length [mm] vs Weight [g] by Species and Watershed",
    x = "Length (mm)",
    y = "Weight (g)"
  ) +
  theme_minimal()


# size & weight along the reaches

# Geographic coordinates x and y from the spa data frame (Script1)
plot(loc$Longitude, loc$Latitude,
     asp = 1,
     pch = 1,
     cex = trouts$Length /100,
     main = "Site Locations and Length",
     xlab = "Longitude",
     ylab = "Latitude")

# Geographic coordinates x and y from the spa data frame (Script1)
plot(loc$Longitude, loc$Latitude,
     asp = 1,
     pch = 1,
     cex = trouts$Weight /100,
     main = "Site Locations and Weight",
     xlab = "Longitude",
     ylab = "Latitude")

#Geographic coordinates x and y from the spa data frame (Script1)
plot(loc$Longitude, loc$Latitude,
     asp = 1,
     pch = 16,
     cex = trouts$Length / 100,
     col = rgb(1, 0, 0, 0.5),
     main = "Site Locations: Length (red) and Weight (blue)",
     xlab = "Longitude",
     ylab = "Latitude")

points(loc$Longitude, loc$Latitude,
       pch = 16,
       cex = trouts$Weight / 100,
       col = rgb(0, 0, 1, 0.5))

model_exp <- lm(log(Weight) ~ log(Length), data = trouts)
summary(model_exp)
a <- exp(coef(model_exp)[1])
b <- coef(model_exp)[2]

cat("Weight =", a, "* Length^", b, "\n")

plot(trouts$Length, trouts$Weight,
     pch = 16,
     xlab = "Length",
     ylab = "Weight")

curve(a * x^b,
      add = TRUE,
      col = "blue",
      lwd = 1,
      lty = 2
      )

describeBy(
  trouts[, c("Length", "Weight", "Effort")],
  group = trouts$Species
)
describeBy(
  trouts[, c("Length", "Weight", "Effort")],
  group = interaction(trouts$Species, trouts$Watershed)
)

library(dplyr)

trouts %>%
  group_by(Species, Watershed) %>%
  summarise(
    n = n(),
    mean_length = mean(Length, na.rm = TRUE),
    sd_length = sd(Length, na.rm = TRUE),
    mean_weight = mean(Weight, na.rm = TRUE),
    sd_weight = sd(Weight, na.rm = TRUE),
    mean_effort = mean(Effort, na.rm = TRUE),
    sd_effort = sd(Effort, na.rm = TRUE)
  )

# Ctrl & shift & C
# cor.test(trouts$Length, trouts$Weight)
# cor.test(SAFO$Length, SAFO$Weight)
# cor.test(SATR$Length, SATR$Weight)
# cor.test(SAFO_BH$Length, SAFO_BH$Weight)
# cor.test(SAFO_OW$Length, SAFO_OW$Weight)
# cor.test(SATR_BH$Length, SATR_BH$Weight)
# cor.test(SATR_OW$Length, SATR_OW$Weight)



