library(dplyr)
library(tidyverse)

#data cleaning--------------
#bring in data
species_df <- read.csv("Data/abundance_clean2018-2023.csv")

#remove years other than 2018 and 2023
year.outs <- c("2019", "2021", "2022")
species_df <- species_df %>% filter(!year %in% year.outs)

#remove extra columns
species_df = subset(species_df, select = -c(X,turfID,originSite,
                                            destinationSite,
                                            originPlotID,
                                            date_yyyymmdd,
                                            AOO,
                                            GBIF_citation,
                                            unknownMorpho,
                                            percentCover,
                                            Origin))

#collapse all species records by treatment and add abundance across plots
species_df <- species_df %>%
  group_by(species, year, treatment) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

#add change column
species_df$abundance_change = NA

#add change values


#figure----------

