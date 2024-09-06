#data wrangling and reformatting with Lorah's transplant data
#pivot wider to make comm phylo matrix 

install.packages("lme4") #modelling
install.packages("lmerTest") #provides p values
install.packages("performance") #model diagnostics
install.packages("ggeffects") #easy prediction & comparison
install.packages("sjPlot") #visualize random effects
install.packages("vegan") #community diversity
install.packages("stringr") #to remove spaces

library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(ggeffects)
library(sjPlot)
library(vegan)
library(stringr)
library(dplyr)

#Data cleaning and formatting---------------------------------------------------
#bring in data
abundance_df <- read.csv("occurance2017-2023.csv")
abundance_df$year <- as.factor(abundance_df$year)

#use only these years
ins <- c("2018","2019","2021","2022","2023")

#get rid of extra control plots
outs <- c("untouched","within_site_transplant")
ins_controlonly <- c("netted_untouched","within_site_transplant","untouched" )

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

#filter data for dataframe with control plots only
abundance_df_controlonly <- abundance_df %>% filter(!is.na(treatment),
                                         !species %in% gc.outs,
                                         !originPlotID %in% block.outs,
                                         treatment %in% ins_controlonly,
                                         year %in% ins)


#calculate diversity metrics for each site and treatment using vegan------------ 

##Shannon pooling all plots per treatment#######
#reformat data
comm_matrix <- pivot_wider(abundance_df1, names_from = species, 
                           values_from = occurrenceCount)

#remove extra columns 
comm_matrix = subset(comm_matrix, select = -c(turfID, originSite, destinationSite,
                                              treatment, date_yyyymmdd, 
                                              functionalGroup, unknownMorpho, 
                                              percentCover) )

#add column for year and treatment
comm_matrix$tx_year = NA
comm_matrix$tx_year <- paste(comm_matrix$treatmentOriginGroup, "_",comm_matrix$year)
comm_matrix <- comm_matrix %>% relocate(tx_year)
comm_matrix = subset(comm_matrix, select = -c(year, treatmentOriginGroup) )

#switch plot IDs to row names
comm_matrix1 <- comm_matrix %>%
  group_by(tx_year) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )
comm_matrix1 <- comm_matrix1 %>% column_to_rownames(var = "tx_year")

#calculate shannon diversity
shannon <- diversity(comm_matrix1, index = "shannon")
shannon_plots <- diversity(comm_matrixID, groups = , index = "shannon")

#make dataframe and reformat to use for plot
shannon_df <- as.data.frame(shannon)
shannon_df$tx_year <- row.names(shannon_df)

shannon_df <- shannon_df %>%
  separate(col = tx_year, into = c("tx_site", "year"), sep = " _ ")

###figure for all plots combined (higher diversity)######
shannon_fig <- ggplot(data = shannon_df, aes(x=year, y=shannon))+
  geom_boxplot()+
  facet_wrap(.~ tx_site)+
  theme_bw()
plot(shannon_fig)


##Shannon for each individual plot and averaging by origin site and treatment###
#make matrix with plot IDs
comm_matrix$ID = NA
comm_matrix$ID <- paste(comm_matrix$treatmentOriginGroup, "_",comm_matrix$year, "_",
                        comm_matrix$originPlotID)
comm_matrixID <- comm_matrix %>% relocate(ID)
comm_matrixID = subset(comm_matrixID, select = -c(year, treatmentOriginGroup, X, X.1) )

#replace NAs with 0s
comm_matrixID[is.na(comm_matrixID)] <- 0

#switch plot IDs to row names for the matrix including plot IDs
comm_matrixID <- comm_matrixID %>%
  group_by(ID) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )
comm_matrixID <- comm_matrixID %>% column_to_rownames(var = "ID")

#make dataframe including plot numbers and reformat to use for plot
shannon_df_plotID <- as.data.frame(shannon_plots)
shannon_df_plotID$ID <- row.names(shannon_df_plotID)

shannon_df_plotID <- shannon_df_plotID %>%
  separate(col = ID, into = c("tx_site", "year", "plotID"), sep = " _ ")

###figure including variation between plots of same tx#######
shannon_fig_plots <- ggplot(data = shannon_df_plotID, aes(x=year, y=shannon_plots))+
  geom_boxplot()+
  facet_wrap(.~ tx_site)+
  theme_bw()
plot(shannon_fig_plots)

#calculate simpson's diversity--------------------------------------------------
simpson <- diversity(comm_matrix1, index = "simpson")

#make dataframe and reformat to use for plot
simpson_df <- as.data.frame(simpson)
simpson_df$tx_year <- row.names(simpson_df)

simpson_df <- simpson_df %>%
  separate(col = tx_year, into = c("tx_site", "year"), sep = " _ ")

#plot
simpson_fig <- ggplot(data = simpson_df, aes(x=year, y=simpson))+
  geom_boxplot()+
  facet_wrap(.~ tx_site)+
  theme_bw()
plot(simpson_fig)
             