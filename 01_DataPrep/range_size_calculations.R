#calculate range size to use in LMMs

library(rgbif) #to get gbif data
library(dplyr) #manipulate data
library(red) #range size calculations
library(CoordinateCleaner) # clean up coordinates

#download data from GBIF--------------------------

#get taxon key 
name_backbone("Achillea_millefolium")

countries <- c("US","MX","CA")

#download occurrence data from GBIF directly

occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"),
  pred_in("country", c("US","CA","MX")),
  pred_gte("year",1990),
  pred("taxonKey",3120060),
  format = "SIMPLE_CSV",
  user="leah.veldhuisen", 
  pwd="Columbia2305", 
  email="leah.veldhuisen@gmail.com")

d <- occ_download_get('0000345-260120142942310') %>%
  occ_download_import()

##clean up data list to only have species name, lat and long##############

df1 <- d %>%
  select(species, decimalLatitude, decimalLongitude,coordinateUncertaintyInMeters,countryCode)
df1 <- df1[df1$coordinateUncertaintyInMeters < 1000, ]
df1 <- na.omit(df1)
df1 <- df1 %>%
  select(species, decimalLatitude, decimalLongitude)
df1$species <- sub(" ",".", df1$species)

#clean points 
flags <- clean_coordinates(x = df1, 
                  lon = "decimalLongitude", 
                  lat = "decimalLatitude", 
                  species = "species")

summary(flags)
plot(flags, lon = "decimalLongitude", lat = "decimalLatitude")

#Exclude problematic records
dat_cl <- df1[flags$.summary,]

#extract species occurrences
species_occ <- dat_cl

#get occurrence points only
points_species <- data.frame(species_occ[,c("decimalLongitude", "decimalLatitude")])

#calculate AOO and EOO with red package----------------------------
aoo(points_species)
