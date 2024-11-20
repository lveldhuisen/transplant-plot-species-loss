library(dplyr)
library(tidyverse)

#data cleaning--------------
#bring in data
abundance_df1 <- read.csv("Data/abundance_clean2018-2023.csv")

#remove years other than 2018 and 2023
year.outs <- c("2019", "2021", "2022")
tx.outs <- c("netted_untouched","untouched")
abundance_df1 <- abundance_df1 %>% filter(!year %in% year.outs,
                                    !treatment %in% tx.outs)

#remove extra columns
abundance_df1 = subset(abundance_df1, select = -c(X,turfID,originSite,
                                            destinationSite,
                                            originPlotID,
                                            date_yyyymmdd,
                                            AOO,
                                            GBIF_citation,
                                            unknownMorpho,
                                            percentCover,
                                            Origin, 
                                            treatmentOriginGroup))

#collapse all species records by treatment and add abundance across plots
species_df <- abundance_df1 %>%
  group_by(species, year, treatment) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

#create new column for 2018 only
species_df18 <- species_df %>% mutate(occ_2018 = ifelse(year == "2018", occurrenceCount, NA))

species_df18 = subset(species_df18, select = -c(occurrenceCount, year))

#create new column for 2023 only
species_df23 <- species_df %>% mutate(occ_2023 = ifelse(year == "2023", occurrenceCount, NA))

species_df23 = subset(species_df23, select = -c(occurrenceCount, year))

##collapse 2018 and 2023 abundances into correct columns
test <- species_df18 %>% left_join(species_df23, by = c("species", "treatment"),relationship = "many-to-many")





#figure----------

