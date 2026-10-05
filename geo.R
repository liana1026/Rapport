path_fishsites <- "C:/Users/liama/Documents/GitHub/MSR/Catoctin_FishSites.csv"
path_loggersites <- "C:/Users/liama/Documents/GitHub/MSR/Catoctin_LoggerSites.csv"
fishsites <- read.csv(path_fishsites)
loggersites <- read.csv(path_loggersites)

library(sf)
library(ggplot2)
library(ggspatial)

id_col_fish  <- names(fishsites)[1]
lat_col_fish <- "Latitude"
lon_col_fish <- "Longitude"
fish_sites_array <- st_as_sf(fishsites, coords = c(lon_col_fish, lat_col_fish), crs = 4326)

id_col_logger  <- names(loggersites)[1]
lat_col_logger <- "Latitude"
lon_col_logger <- "Longitude"
logger_sites_array <- st_as_sf(loggersites, coords = c(lon_col_logger, lat_col_logger), crs = 4326)

map_fish <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 14) + 
  geom_sf(data = fish_sites_array, color = "red", size = 1.5) +
  geom_sf_text(data = fish_sites_array, aes(label = .data[[id_col_fish]]), size = 2.5, nudge_y = 0.003, check_overlap = TRUE) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal()) +
  labs(title = "Fish sampling sites, Catoctin Mountain Park", x = "Longitude", y = "Latitude") +
  theme_bw()
map_fish

map_logger <- ggplot() +
  annotation_map_tile(type = "osm", zoom = 14) + 
  geom_sf(data = logger_sites_array, color = "red", size = 1.5) +
  geom_sf_text(data = logger_sites_array, aes(label = .data[[id_col_logger]]), size = 2.5, nudge_y = 0.003, check_overlap = TRUE) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal()) +
  labs(title = "Logger sites, Catoctin Mountain Park", x = "Longitude", y = "Latitude") +
  theme_bw()
map_logger

library(sf)
library(ggplot2)
library(maps)

# Approximate center of Catoctin Mountain Park
catoctin <- data.frame(
  Longitude = -77.45,
  Latitude  = 39.63
)

usa <- map_data("state")

overview_map <- ggplot() +
  geom_polygon(
    data = usa,
    aes(long, lat, group = group),
    fill = "grey90",
    color = "grey50",
    linewidth = 0.2
  ) +
  geom_point(
    data = catoctin,
    aes(Longitude, Latitude),
    color = "red",
    size = 3
  ) +
  annotate(
    "text",
    x = -77.45,
    y = 39.63 + 3,
    label = "Catoctin Mountain Park",
    size = 4
  ) +
  coord_fixed(1.3) +
  labs(
    title = "Location of Catoctin Mountain Park within the USA",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_bw()

overview_map

