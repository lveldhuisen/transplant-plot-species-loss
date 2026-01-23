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

# bring in gps data ----------
gps <- 
register_stadiamaps("", write = TRUE)

# Get US map data
us_map <- map_data("state")

# Gothic, Colorado coordinates
gothic_lat <- 38.9597
gothic_lon <- -106.9878

# Create the map
map <- ggplot() +
  geom_polygon(data = us_map, 
               aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  geom_point(aes(x = gothic_lon, y = gothic_lat),
             shape = 8, size = 5, color = "red", stroke = 2) +
  annotation_scale(location = "bl", width_hint = 0.3) +  # scale bar bottom-left
  annotation_north_arrow(location = "tr", which_north = "true",  # north arrow top-right
                         style = north_arrow_fancy_orienteering) +
  coord_fixed(1.3) +
  theme_minimal(base_size = 18) +
  labs(x = "Longitude",
       y = "Latitude", 
       size = 20)

plot(map)
ggsave("Figures/map.png", dpi = 600, height = 8, width = 15)
