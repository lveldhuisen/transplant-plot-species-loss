library(tidyverse)
library(ggmap)
library(osmdata)
library(ggspatial)
library(tmap)
library(grid)
library(cowplot)
library(sf)
library(rnaturalearth)


# API key
register_google(key = "AIzaSyDuTze63zSNtxg7BGSvpY0I-QRLK-q4OS4", write = TRUE)
register_stadiamaps("5673664f-130e-4a27-91df-b6c2d07ffd53", write = TRUE)


# Get map data and convert to sf
us_map <- map_data("state")
us_sf <- st_as_sf(us_map, coords = c("long", "lat"), crs = 4326) %>%
  group_by(group) %>%
  summarize(do_union = FALSE) %>%
  st_cast("POLYGON")

# Project to Albers Equal Area (good for US maps)
us_sf <- st_transform(us_sf, crs = 5070)

# Create Gothic point and project
gothic <- st_sfc(st_point(c(-106.9878, 38.9597)), crs = 4326)
gothic <- st_transform(gothic, crs = 5070)

map <- ggplot() +
  geom_sf(data = us_sf, fill = "lightgray", color = "white") +
  geom_sf(data = gothic, shape = 8, size = 5, color = "red", stroke = 2) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", 
                         which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_minimal(base_size = 14)

plot(map)
ggsave("Figures/map.png", dpi = 600, height = 8, width = 15)
