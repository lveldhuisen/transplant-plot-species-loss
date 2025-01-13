library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(ggpmisc)

#Data formatting--------------

#bring in data
abundance_df1 <- read.csv("Data/abundance_clean2018-2023.csv")
abundance_df1$year <- as.factor(abundance_df1$year)

#use only within site transplant for control, get rid of unknowns and remove
#extra years

control.outs <- c("netted_untouched","untouched")
gc.outs <- c("litter", "bare_soil", "rock", "moss","unknown_seedling",
             "unknown_forb","unknown_grass")
#year.outs <- c("2019","2021","2022")
abundance_df1 <- abundance_df1 %>% filter(!is.na(treatment),
                                          !treatment %in% control.outs,
                                          !species %in% gc.outs)
                                          #!year %in% year.outs)
#remove extra columns
abundance_df1 = subset(abundance_df1, select = -c(X,
                                                  date_yyyymmdd,
                                                  unknownMorpho,
                                                  percentCover,
                                                  Origin,
                                                  GBIF_citation,
                                                  functionalGroup))

#Change from 2018 to 2023 abundance---------------
##Make separate data frames for each origin/tx combo-------
##Origin: Upper Montane####
um_win <- abundance_df1 %>% filter(originSite == "Upper Montane",
                                   treatment == "within_site_transplant")

um_c1 <- abundance_df1 %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_one_step")
  
um_c2 <- abundance_df1 %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_two_steps")
   
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

##Upper Montane#######

###within site transplant#####
um_win_reg <- um_win %>% 
  group_by(species,year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

um_win_reg$year <- as.numeric(um_win_reg$year)

models <- um_win_reg %>%
  group_by(species) %>%
  do(model = lm(occurrenceCount ~ year, data = um_win_reg))

# Print the models

coefficients_summary <- models %>%
  summarise(
    intercept = coef(model)[1],
    slope = coef(model)[2]
  )

# Print the summary
print(coefficients_summary)

ggplot(um_win_reg,
       aes(x = year, y = occurrenceCount)) +
  geom_point() +
  # add regression lines
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~species)+
  stat_poly_eq(use_label(c("eq")))

###cooled 1#####
um_c1_reg <- um_c1 %>% 
  group_by(species,year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

um_c1_reg$year <- as.numeric(um_c1_reg$year)

models <- um_c1_reg %>%
  group_by(species) %>%
  do(model = lm(occurrenceCount ~ year, data = um_c1_reg))


# Print the models

coefficients_summary <- models %>%
  summarise(
    intercept = coef(model)[1],
    slope = coef(model)[2]
  )

# Print the summary
print(coefficients_summary)

ggplot(um_c1_reg,
       aes(x = year, y = occurrenceCount)) +
  geom_point() +
  # add regression lines
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~species)+
  stat_poly_eq(use_label(c("eq")))
