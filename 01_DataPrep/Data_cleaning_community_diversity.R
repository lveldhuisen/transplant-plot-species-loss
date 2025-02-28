library(dplyr)
library(tidyverse)

#Data cleaning and formatting---------------------------------------------------

##all cover data reformatting############

#bring in data
abundance_df <- read.csv("Data/occurance2017-2023.csv")
abundance_df$year <- as.factor(abundance_df$year)

#use only these years
ins <- c("2018","2019","2021","2022","2023")

#get rid of extra control plots
outs <- c("untouched","netted_untouched")

# remove non-species from species column
gc.outs <- c("litter", "bare_soil", "rock", "moss","unknown_seedling",
             "unknown_forb","unknown_grass", "unknown_round_leaves")

#remove block 6 plots since they were transplanted in 2018
block.outs <- c("mo6-1", "mo6-2", "mo6-3", "mo6-4","mo6-5", "pf6-1",
                "pf6-2", "pf6-3","pf6-4", "um6-1", "um6-2","um6-3","um6-4",
                "um6-5","um6-6")

#filter data for things you never want
abundance_df1 <- abundance_df %>% filter(!is.na(treatment),
                                         !species %in% gc.outs,
                                         !originPlotID %in% block.outs,
                                         !treatment %in% outs,
                                         year %in% ins)
#get rid of extra X columns
abundance_df1 = subset(abundance_df1, select = -c(X,X.1))

abundance_df1$percentCover <- as.numeric(abundance_df1$percentCover)/100

cover_df <- abundance_df1

#save clean vertically-formatted abundance data
write.csv(cover_df, file = "cover_clean2018-2023.csv")

##count species in each site (including all control treatments and 2017)####
abundance_forcounts <- abundance_df %>% filter(!is.na(treatment),
                                         !species %in% gc.outs,)

abundance_forcounts %>%
  group_by(originSite) %>%
  summarise(UniqueCount = n_distinct(species))

abundance_forcounts %>%
  summarise(UniqueCount = n_distinct(species))

###reformat to matrix for Vegan########

#reformat data
comm_matrix <- pivot_wider(cover_df, names_from = species, 
                           values_from = percentCover)

#make matrix with plot IDs
comm_matrix$ID = NA
comm_matrix$ID <- paste(comm_matrix$turfID,"_", comm_matrix$originSite,"_",
                        comm_matrix$destinationSite,
                        "_",comm_matrix$treatment,
                        "_",comm_matrix$year)
comm_matrixID <- comm_matrix %>% relocate(ID)
comm_matrixID = subset(comm_matrixID, select = -c(turfID,originSite, destinationSite,
                                                  originPlotID, 
                                                  treatment,treatmentOriginGroup,year,
                                                  date_yyyymmdd, functionalGroup,
                                                  unknownMorpho, occurrenceCount) )

#replace NAs with 0s
comm_matrixID[is.na(comm_matrixID)] <- 0

#sum to put all sites in same row
comm_matrixID <- comm_matrixID %>%
  group_by(ID) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm = FALSE
  )

comm_matrixID <- comm_matrixID %>% column_to_rownames(var = "ID")

#save as csv 
write.csv(comm_matrixID, file = "Data/Community_matrix_ByPlot.csv")

##make abundance dataframe with only the three control treatments##############

#use only control plots
ins_controlonly <- c("netted_untouched","within_site_transplant","untouched" )

#filter data for dataframe with control plots only
abundance_df_controlonly <- abundance_df %>% filter(!is.na(treatment),
                                                    !species %in% gc.outs,
                                                    !originPlotID %in% block.outs,
                                                    treatment %in% ins_controlonly,
                                                    year %in% ins)

###reformat data to matrix for vegan###############
comm_matrix_controls <- pivot_wider(abundance_df_controlonly, names_from = species, 
                                    values_from = occurrenceCount)

#remove extra columns 
comm_matrix_controls = subset(comm_matrix_controls, select = -c(originSite, 
                                                                destinationSite,date_yyyymmdd, 
                                                                functionalGroup, unknownMorpho, 
                                                                percentCover) )

#make matrix with plot IDs
comm_matrix_controls$ID = NA

comm_matrix_controls$ID <- paste(comm_matrix_controls$treatmentOriginGroup, "_",comm_matrix_controls$year, "_", comm_matrix_controls$originPlotID, "_", comm_matrix_controls$treatment)

comm_matrix_controls <- comm_matrix_controls %>% relocate(ID)
comm_matrix_controls = subset(comm_matrix_controls, select = -c(year, treatmentOriginGroup, X, X.1, treatment) )

#replace NAs with 0s
comm_matrix_controls[is.na(comm_matrix_controls)] <- 0

#switch plot IDs to row names for the matrix including plot IDs
comm_matrix_controls <- comm_matrix_controls %>%
  group_by(ID) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )
comm_matrix_controls <- comm_matrix_controls %>% column_to_rownames(var = "ID")

write.csv(comm_matrix_controls, file = "Data/Community_matrix_controlsonly.csv")

#Calculate how species lost and gained from 2017-2023 for each plot-------------

#look at only 2017 and 2023 for beginning and end
years <- c("2017","2023")

richness_df <- abundance_df %>% filter(year %in% years,
                                       !treatment %in% outs,
                                       !species %in% gc.outs,
                                       !turfID %in% block.outs,
                                       !is.na(treatment))
#get rid of extra columns
richness_df = subset(richness_df, select = -c(X,
                                              X.1, 
                                              date_yyyymmdd, 
                                              unknownMorpho,
                                              functionalGroup, 
                                              percentCover, 
                                              occurrenceCount))
#count species
plot_richness <- richness_df %>%
  group_by(turfID, year) %>%
  summarise(UniqueCount = n_distinct(species))

#separate into 2018 and 2023 datasets 
richness_df_2017 <- richness_df %>% filter(year %in% 2017)
richness_df_2018 <- richness_df %>% filter(year %in% 2018)
richness_df_2023 <- richness_df %>% filter(year %in% 2023)

#see which species do not occur in both 
species_only_2017 <- richness_df_2017 %>% 
  group_by(turfID) %>%
  filter(!species %in% richness_df_2023$species)

species_only_2018 <- richness_df_2018 %>% 
  group_by(turfID) %>%
  filter(!species %in% richness_df_2023$species)

species_only_2023 <- richness_df_2023 %>% 
  group_by(turfID) %>%
  filter(!species %in% richness_df_2018$species)
