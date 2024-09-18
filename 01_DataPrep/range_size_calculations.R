#calculate range size to use in LMMs

library(rgbif) #to get gbif data
library(dplyr) #manipulate data
library(red) #range size calculations

#download data from GBIF--------------------------

#get taxon key 
name_backbone("Collomia_linearis")

countries <- c("US","MX","CA")

#download occurrence data from GBIF directly

occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"),
  pred_in("country", c("US","CA","MX")),
  pred_gte("year",1990),
  pred("taxonKey",2927552),
  format = "SIMPLE_CSV",
  user="leah.veldhuisen", 
  pwd="Columbia2305", 
  email="leah.veldhuisen@gmail.com")

d <- occ_download_get('0021562-240906103802322') %>%
  occ_download_import()

#pred_in("country", c("US","CA","MX")),
##clean up data list to only have species name, lat and long##############

df1 <- d %>%
  select(species, decimalLatitude, decimalLongitude,coordinateUncertaintyInMeters,countryCode)
df1 <- df1[df1$coordinateUncertaintyInMeters < 1000, ]
df1 <- na.omit(df1)
df1 <- df1 %>%
  select(species, decimalLatitude, decimalLongitude)
df1$species <- sub(" ",".", df1$species)

#extract species occurrences
species_occ <- df1

#get occurrence points only
points_species <- data.frame(species_occ[,c("decimalLongitude", "decimalLatitude")])

#calculate AOO and EOO with red package----------------------------
aoo(points_species)
