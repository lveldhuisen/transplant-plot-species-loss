library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(ggpmisc)
library(plyr)

#bring in data
abundance_df1 <- read.csv("Data/abundance_clean2018-2023.csv")
abundance_df1$year <- as.factor(abundance_df1$year)

#Format big combined dataset--------

#use only within site transplant for control, get rid of unknowns and remove
#extra years

control.outs <- c("netted_untouched","untouched")
gc.outs <- c("litter", "bare_soil", "rock", "moss","unknown_seedling",
             "unknown_forb","unknown_grass")

abundance_df1 <- abundance_df1 %>% filter(!is.na(treatment),
                                          !treatment %in% control.outs,
                                          !species %in% gc.outs)
#remove extra columns
abundance_df1 = subset(abundance_df1, select = -c(X,
                                                  date_yyyymmdd,
                                                  unknownMorpho,
                                                  percentCover,
                                                  Origin,
                                                  GBIF_citation,
                                                  functionalGroup))

##Make separate data frames for each origin/tx combo-------

##Origin: Upper Montane####
um_win <- abundance_df1 %>% filter(originSite == "Upper Montane",
                                   treatment == "within_site_transplant")
write.csv(um_win, "Data/Species_change/UM_withinsite.csv")

um_c1 <- abundance_df1 %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_one_step")
write.csv(um_c1, "Data/Species_change/UM_cooled1.csv")
  
um_c2 <- abundance_df1 %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_two_steps")
write.csv(um_c2, "Data/Species_change/UM_cooled2.csv")
   
##Origin: Pfeiler#####
pf_win <- abundance_df1 %>% filter(originSite == "Pfeiler",
                                   treatment == "within_site_transplant")

pf_w1 <- abundance_df1 %>% filter(originSite == "Pfeiler",
                                 treatment == "warmed_one_step")
  
pf_c1 <- abundance_df1 %>% filter(originSite == "Pfeiler",
                                  treatment == "cooled_one_step")

##Origin: Monument####
mo_win <- abundance_df1 %>% filter(originSite == "Monument",
                                  treatment == "within_site_transplant")
  
mo_w1 <- abundance_df1 %>% filter(originSite == "Monument",
                                 treatment == "warmed_one_step")
  
mo_w2 <- abundance_df1 %>% filter(originSite == "Monument",
                                  treatment == "warmed_two_steps")


#Regression to get slope as metric of change-----------------

#added all zeroes for years where species were not observed in Excel#

##Upper Montane#######

###within site transplant#####
um_win_reg <- read.csv("Data/Species_change/UM_withinsite.csv")

#total across plots
um_win_reg <- um_win_reg %>% 
  group_by(species,year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

um_win_reg$year <- as.numeric(um_win_reg$year) #make year numeric

#do regression 
um_win_model <- dlply(um_win_reg,"species",function(um_win_reg) lm(occurrenceCount ~ year, 
                                                           data = um_win_reg))
um_win_values <- ldply(um_win_model,coef)

#make plot for visual
ggplot(um_win_reg,
       aes(x = year, y = occurrenceCount)) +
  geom_point() +
  # add regression lines
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~species)+
  stat_poly_eq(use_label(c("eq")))

###cooled 1#####
um_c1_reg <- read.csv("Data/Species_change/UM_cooled1.csv")

#total across plots
um_c1_reg <- um_c1_reg %>% 
  group_by(species,year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

um_c1_reg$year <- as.numeric(um_c1_reg$year) #make year numeric

#do regression 
um_c1_model <- dlply(um_c1_reg,"species",function(um_c1_reg) lm(occurrenceCount ~ year, 
                                                                   data = um_c1_reg))
um_c1_values <- ldply(um_c1_model,coef)

#plot to check 
ggplot(um_c1_reg,
       aes(x = year, y = occurrenceCount)) +
  geom_point() +
  # add regression lines
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~species)+
  stat_poly_eq(use_label(c("eq")))

#Format each table to include treatment, origin site, remove NAs-----------


