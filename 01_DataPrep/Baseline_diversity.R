library(dplyr)
library(tidyverse)
library(data.table)
library(vegan) #community diversity
library(stringr) #to remove spaces

#bring in data
abundance_df <- read.csv("Data/occurance2017-2023.csv")
abundance_df$year <- as.factor(abundance_df$year)

#reformat for richness calculation ----

#use only these years
ins <- c("2017")

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
abundance_17_df <- abundance_df %>% filter(!is.na(treatment),
                                         !species %in% gc.outs,
                                         !originPlotID %in% block.outs,
                                         !treatment %in% outs,
                                         year %in% ins)

#get rid of extra X columns
abundance_17_df = subset(abundance_17_df, select = -c(X,X.1, percentCover))

###reformat to matrix for Vegan########

#reformat data
comm_matrix_2017 <- pivot_wider(abundance_17_df, names_from = species, 
                           values_from = occurrenceCount)

#make matrix with plot IDs
comm_matrix_2017$ID = NA
comm_matrix_2017$ID <- paste(comm_matrix_2017$turfID,"_", comm_matrix_2017$originSite,"_",
                             comm_matrix_2017$destinationSite,
                        "_",comm_matrix_2017$treatment)
comm_matrix_2017 <- comm_matrix_2017 %>% relocate(ID)
comm_matrix_2017 = subset(comm_matrix_2017, select = -c(turfID,originSite, destinationSite,
                                                  originPlotID, 
                                                  treatment,treatmentOriginGroup,year,
                                                  date_yyyymmdd, functionalGroup,
                                                  unknownMorpho) )

#replace NAs with 0s
comm_matrix_2017[is.na(comm_matrix_2017)] <- 0

#sum to put all sites in same row
comm_matrix_2017 <- comm_matrix_2017 %>%
  group_by(ID) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm = FALSE
  )

comm_matrix_2017 <- comm_matrix_2017 %>% column_to_rownames(var = "ID")


#species richness-----------------
richness_baseline <- specnumber(comm_matrix_2017, groups = comm_matrix_2017$ID, MARGIN = 1)
richness__baseline <- as.data.frame(richness_baseline)    
write.csv(richness_baseline, "Data/baseline_richness_data.csv")
