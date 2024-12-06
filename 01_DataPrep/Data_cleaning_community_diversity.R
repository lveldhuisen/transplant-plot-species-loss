library(dplyr)
library(tidyverse)

#Data cleaning and formatting---------------------------------------------------

##all abundance data reformatting############

#bring in data
abundance_df <- read.csv("Data/occurance2017-2023.csv")
abundance_df$year <- as.factor(abundance_df$year)

#use only these years
ins <- c("2018","2019","2021","2022","2023")

#get rid of extra control plots
outs <- c("untouched","netted_untouched")

# remove non-species from species column
gc.outs <- c("litter", "bare_soil", "rock")

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

#save clean vertically-formatted abundance data
write.csv(abundance_df1, file = "abundance_clean2018-2023.csv")

###reformat to matrix for Vegan########

#reformat data
comm_matrix <- pivot_wider(abundance_df1, names_from = species, 
                           values_from = occurrenceCount)

#make matrix with plot IDs
comm_matrix$ID = NA
comm_matrix$ID <- paste(comm_matrix$treatmentOriginGroup, "_",comm_matrix$year,
                        "_", comm_matrix$originPlotID)
comm_matrixID <- comm_matrix %>% relocate(ID)
comm_matrixID = subset(comm_matrixID, select = -c(turfID,originSite, destinationSite,
                                                  originPlotID, 
                                                  treatment,treatmentOriginGroup,year,
                                                  date_yyyymmdd, functionalGroup,
                                                  unknownMorpho, percentCover) )

#replace NAs with 0s
comm_matrixID[is.na(comm_matrixID)] <- 0

#switch plot IDs to row names for the matrix including plot IDs
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


