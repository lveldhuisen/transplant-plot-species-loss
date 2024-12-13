library(dplyr)
library(tidyverse)

#Data formatting--------------

#bring in data
abundance_df1 <- read.csv("Data/abundance_clean2018-2023.csv")
abundance_df1$year <- as.factor(abundance_df1$year)

#use only within site transplant for control, get rid of unknowns and remove
#extra years

control.outs <- c("netted_untouched","untouched")
gc.outs <- c("litter", "bare_soil", "rock", "moss","unknown_seedling",
             "unknown_forb","unknown_grass")
year.outs <- c("2019","2021","2022")
abundance_df1 <- abundance_df1 %>% filter(!is.na(treatment),
                                          !treatment %in% control.outs,
                                          !species %in% gc.outs,
                                          !year %in% year.outs)
#remove extra columns
abundance_df1 = subset(abundance_df1, select = -c(X,
                                                  date_yyyymmdd,
                                                  unknownMorpho,
                                                  percentCover,
                                                  Origin,
                                                  GBIF_citation,
                                                  functionalGroup))


#Make separate data frames for each origin/tx combo-------
##Upper Montane####
um_win <- abundance_df1 %>% filter(originSite == "Upper Montane",
                                   treatment == "within_site_transplant")

um_c1 <- abundance_df1 %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_one_step")
  
um_c2 <- abundance_df1 %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_two_steps")
   
##Pfeiler#####
pf_win <- abundance_df1 %>% filter(originSite == "Pfeiler",
                                   treatment == "within_site_transplant")

pf_w1 <- abundance_df1 %>% filter(originSite == "Pfeiler",
                                 treatment == "warmed_one_step")
  
pf_c2 <- abundance_df1 %>% filter(originSite == "Pfeiler",
                                  treatment == "cooled_one_step")

##Monument####
mo_win <- abundance_df1 %>% filter(originSite == "Monument",
                                  treatment == "within_site_transplant")
  
mo_w1 <- abundance_df1 %>% filter(originSite == "Monument",
                                 treatment == "warmed_one_step")
  
mo_w2 <- abundance_df1 %>% filter(originSite == "Monument",
                                  treatment == "warmed_two_steps")
