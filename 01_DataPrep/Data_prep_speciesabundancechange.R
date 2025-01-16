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
write.csv(pf_win, "Data/Species_change/Pfeiler_withinsite_raw.csv")

pf_w1 <- abundance_df1 %>% filter(originSite == "Pfeiler",
                                 treatment == "warmed_one_step")
write.csv(pf_w1, "Data/Species_change/Pfeiler_w1_raw.csv")
  
pf_c1 <- abundance_df1 %>% filter(originSite == "Pfeiler",
                                  treatment == "cooled_one_step")
write.csv(pf_c1, "Data/Species_change/Pfeiler_c1_raw.csv")

##Origin: Monument####
mo_win <- abundance_df1 %>% filter(originSite == "Monument",
                                  treatment == "within_site_transplant")
write.csv(mo_win, "Data/Species_change/Monument_within_raw.csv")
  
mo_w1 <- abundance_df1 %>% filter(originSite == "Monument",
                                 treatment == "warmed_one_step")
write.csv(mo_w1, "Data/Species_change/Monument_w1_raw.csv")
  
mo_w2 <- abundance_df1 %>% filter(originSite == "Monument",
                                  treatment == "warmed_two_steps")
write.csv(mo_w2, "Data/Species_change/Monument_w2_raw.csv")

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

###cooled 2####
um_c2_reg <- read.csv("Data/Species_change/UM_cooled2.csv")

#total across plots
um_c2_reg <- um_c2_reg %>% 
  group_by(species,year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

um_c2_reg$year <- as.numeric(um_c2_reg$year) #make year numeric

#do regression 
um_c2_model <- dlply(um_c2_reg,"species",function(um_c2_reg) lm(occurrenceCount ~ year, 
                                                                data = um_c2_reg))
um_c2_values <- ldply(um_c2_model,coef)

##Pfeiler######

###within site transplant####
pf_win_reg <- read.csv("Data/Species_change/Pfeiler_withinsite_raw.csv")

#total across plots
pf_win_reg <- pf_win_reg %>% 
  group_by(species,year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

pf_win_reg$year <- as.numeric(pf_win_reg$year) #make year numeric

#do regression 
pf_win_model <- dlply(pf_win_reg,"species",function(pf_win_reg) lm(occurrenceCount ~ year, 
                                                                   data = pf_win_reg))
pf_win_values <- ldply(pf_win_model,coef)

###cooled one####
pf_c1_reg <- read.csv("Data/Species_change/Pfeiler_c1_raw.csv")

#total across plots
pf_c1_reg <- pf_c1_reg %>% 
  group_by(species,year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

pf_c1_reg$year <- as.numeric(pf_c1_reg$year) #make year numeric

#do regression 
pf_c1_model <- dlply(pf_c1_reg,"species",function(pf_c1_reg) lm(occurrenceCount ~ year, 
                                                                   data = pf_c1_reg))
pf_c1_values <- ldply(pf_c1_model,coef)

###warmed one#####
pf_w1_reg <- read.csv("Data/Species_change/Pfeiler_w1_raw.csv")

#total across plots
pf_w1_reg <- pf_w1_reg %>% 
  group_by(species,year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

pf_w1_reg$year <- as.numeric(pf_w1_reg$year) #make year numeric

#do regression 
pf_w1_model <- dlply(pf_w1_reg,"species",function(pf_w1_reg) lm(occurrenceCount ~ year, 
                                                                data = pf_w1_reg))
pf_w1_values <- ldply(pf_w1_model,coef)

##Monument######

###within site transplant#####
mo_win_reg <- read.csv("Data/Species_change/Monument_within_raw.csv")

#total across plots
mo_win_reg <- mo_win_reg %>% 
  group_by(species,year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

mo_win_reg$year <- as.numeric(mo_win_reg$year) #make year numeric

#do regression 
mo_win_model <- dlply(mo_win_reg,"species",function(mo_win_reg) lm(occurrenceCount ~ year, 
                                                                data = mo_win_reg))
mo_win_values <- ldply(mo_win_model,coef)

###warmed one#####
mo_w1_reg <- read.csv("Data/Species_change/Monument_w1_raw.csv")

#total across plots
mo_w1_reg <- mo_w1_reg %>% 
  group_by(species,year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

mo_w1_reg$year <- as.numeric(mo_w1_reg$year) #make year numeric

#do regression 
mo_w1_model <- dlply(mo_w1_reg,"species",function(mo_w1_reg) lm(occurrenceCount ~ year, 
                                                                   data = mo_w1_reg))
mo_w1_values <- ldply(mo_w1_model,coef)

###warmed two#####
mo_w2_reg <- read.csv("Data/Species_change/Monument_w2_raw.csv")

#total across plots
mo_w2_reg <- mo_w2_reg %>% 
  group_by(species,year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

mo_w2_reg$year <- as.numeric(mo_w2_reg$year) #make year numeric

#do regression 
mo_w2_model <- dlply(mo_w2_reg,"species",function(mo_w2_reg) lm(occurrenceCount ~ year, 
                                                                data = mo_w2_reg))
mo_w2_values <- ldply(mo_w2_model,coef)

#Format each table to include treatment, origin site, remove NAs-----------

##Upper Montane####
###within site####

#rename columns 
colnames(um_win_values)[colnames(um_win_values) == '(Intercept)'] <- 'intercept'
colnames(um_win_values)[colnames(um_win_values) == 'year'] <- 'slope'

#get rid of intercept column
um_win_values = subset(um_win_values, select = -c(intercept))

#add columns for treatment and origin site
um_win_values$originSite = "Upper Montane"
um_win_values$treatment = "within_site_transplant"

#save as csv
write_csv(um_win_values, "Data/Species_change/UM_win_slopes.csv")

###cooled 1####
#rename columns 
colnames(um_c1_values)[colnames(um_c1_values) == '(Intercept)'] <- 'intercept'
colnames(um_c1_values)[colnames(um_c1_values) == 'year'] <- 'slope'

#get rid of intercept column
um_c1_values = subset(um_c1_values, select = -c(intercept))

#add columns for treatment and origin site
um_c1_values$originSite = "Upper Montane"
um_c1_values$treatment = "cooled_one"

#save as csv
write_csv(um_c1_values, "Data/Species_change/UM_c1_slopes.csv")

###cooled 2#####
#rename columns 
colnames(um_c2_values)[colnames(um_c2_values) == '(Intercept)'] <- 'intercept'
colnames(um_c2_values)[colnames(um_c2_values) == 'year'] <- 'slope'

#get rid of intercept column
um_c2_values = subset(um_c2_values, select = -c(intercept))

#add columns for treatment and origin site
um_c2_values$originSite = "Upper Montane"
um_c2_values$treatment = "cooled_two"

#save as csv
write_csv(um_c2_values, "Data/Species_change/UM_c2_slopes.csv")

##Pfeiler#####

###within site transplant#####
#rename columns 
colnames(pf_win_values)[colnames(pf_win_values) == '(Intercept)'] <- 'intercept'
colnames(pf_win_values)[colnames(pf_win_values) == 'year'] <- 'slope'

#get rid of intercept column
pf_win_values = subset(pf_win_values, select = -c(intercept))

#add columns for treatment and origin site
pf_win_values$originSite = "Pfeiler"
pf_win_values$treatment = "within_site_transplant"

#save as csv
write_csv(pf_win_values, "Data/Species_change/Pfeiler_win_slopes.csv")

###cooled 1#####
#rename columns 
colnames(pf_c1_values)[colnames(pf_c1_values) == '(Intercept)'] <- 'intercept'
colnames(pf_c1_values)[colnames(pf_c1_values) == 'year'] <- 'slope'

#get rid of intercept column
pf_c1_values = subset(pf_c1_values, select = -c(intercept))

#add columns for treatment and origin site
pf_c1_values$originSite = "Pfeiler"
pf_c1_values$treatment = "cooled_one"

#save as csv
write_csv(pf_c1_values, "Data/Species_change/Pfeiler_c1_slopes.csv")

###warmed one####
#rename columns 
colnames(pf_w1_values)[colnames(pf_w1_values) == '(Intercept)'] <- 'intercept'
colnames(pf_w1_values)[colnames(pf_w1_values) == 'year'] <- 'slope'

#get rid of intercept column
pf_w1_values = subset(pf_w1_values, select = -c(intercept))

#add columns for treatment and origin site
pf_w1_values$originSite = "Pfeiler"
pf_w1_values$treatment = "warmed_one"

#save as csv
write_csv(pf_w1_values, "Data/Species_change/Pfeiler_w1_slopes.csv")

##Monument#######

###within site transplant#####
#rename columns 
colnames(mo_win_values)[colnames(mo_win_values) == '(Intercept)'] <- 'intercept'
colnames(mo_win_values)[colnames(mo_win_values) == 'year'] <- 'slope'

#get rid of intercept column
mo_win_values = subset(mo_win_values, select = -c(intercept))

#add columns for treatment and origin site
mo_win_values$originSite = "Monument"
mo_win_values$treatment = "within_site_transplant"

#save as csv
write_csv(mo_win_values, "Data/Species_change/Monument_within_slopes.csv")

###warmed one#####

#rename columns 
colnames(mo_w1_values)[colnames(mo_w1_values) == '(Intercept)'] <- 'intercept'
colnames(mo_w1_values)[colnames(mo_w1_values) == 'year'] <- 'slope'

#get rid of intercept column
mo_w1_values = subset(mo_w1_values, select = -c(intercept))

#add columns for treatment and origin site
mo_w1_values$originSite = "Monument"
mo_w1_values$treatment = "warmed_one"

#save as csv
write_csv(mo_w1_values, "Data/Species_change/Monument_w1_slopes.csv")

###warmed two#####

#rename columns 
colnames(mo_w2_values)[colnames(mo_w2_values) == '(Intercept)'] <- 'intercept'
colnames(mo_w2_values)[colnames(mo_w2_values) == 'year'] <- 'slope'

#get rid of intercept column
mo_w2_values = subset(mo_w2_values, select = -c(intercept))

#add columns for treatment and origin site
mo_w2_values$originSite = "Monument"
mo_w2_values$treatment = "warmed_two"

#save as csv
write_csv(mo_w2_values, "Data/Species_change/Monument_w2_slopes.csv")

#Total abundance by plot to test relationship with change--------------

#bring in slopes data
aoo_slopes <- read.csv("Data/Species_change/Abundance_slopes_all.csv")

#add column to have origin site and tx in same column 
aoo_slopes$group <- paste(aoo_slopes$originSite,"_",aoo_slopes$treatment,
                          "_",aoo_slopes$species)
aoo_slopes$occurrenceCount <- NA

#bring in raw abundance data
raw_2017 <- read.csv("Data/occurance2017-2023.csv")

#filter to only 2017
raw_2017 <- raw_2017 %>% filter(year %in% "2017")

#get rid of extra controls
raw_2017 <- raw_2017 %>% filter(!treatment %in% "netted_untouched",
                                !treatment %in% "untouched")

raw_2017 <- raw_2017 %>% filter(!species %in% "Unknown_round_leaves")

#update species names to match phylogeny replacements

raw_2017$species[raw_2017$species == 'Agoseris_glauca'] <- 'Agoseris_glauca_var._dasycephala'
raw_2017$species[raw_2017$species == 'Aquilegia_caerulea'] <- 'Aquilegia_coerulea'
raw_2017$species[raw_2017$species == 'Epilobium_sp.'] <- 'Epilobium_ciliatum'
raw_2017$species[raw_2017$species == 'Erigeron_elatior'] <- 'Erigeron_grandiflorus'
raw_2017$species[raw_2017$species == 'Festuca_rubra'] <- 'Festuca_rubra_subsp._rubra'
raw_2017$species[raw_2017$species == 'Helianthella_quinquenervis'] <- 'Helianthella_uniflora'
raw_2017$species[raw_2017$species == 'Heterotheca_pumila'] <- 'Heterotheca_villosa'
raw_2017$species[raw_2017$species == 'Hydrophyllum_capitatum'] <- 'Hydrophyllum_capitatum_var._capitatum'
raw_2017$species[raw_2017$species == 'Lupinus_sp.'] <- 'Lupinus_argenteus'
raw_2017$species[raw_2017$species == 'Poa_pratensis'] <- 'Poa_pratensis_subsp._pratensis'
raw_2017$species[raw_2017$species == 'Polygonum_douglasii'] <- 'Polygonum_douglasii_subsp._douglasii'
raw_2017$species[raw_2017$species == 'Senecio_integerrimus'] <- 'Senecio_triangularis'
raw_2017$species[raw_2017$species == 'Symphyotrichum_ascendens'] <- 'Symphyotrichum_foliaceum'
raw_2017$species[raw_2017$species == 'Carex_sp.'] <- 'Carex_nelsonii'

#get rid of extra columns
raw_2017 = subset(raw_2017, select = -c(X,
                                        X.1,
                                        percentCover,
                                        unknownMorpho,
                                        functionalGroup,
                                        date_yyyymmdd,
                                        treatmentOriginGroup,
                                        turfID,
                                        originPlotID,
                                        year
))

#add column to have origin site and tx in same column 
raw_2017$group <- paste(raw_2017$originSite,"_",raw_2017$treatment,"_",raw_2017$species)

#collapse values based on groups column and add all occurrences 
raw_2017 <- aggregate(occurrenceCount ~ group, data = raw_2017, FUN = sum)

#merge two datasets

test <- full_join(aoo_slopes, raw_2017, by=c("group"))
test <- test[!is.na(test$slope),]

write.csv(test, "Data/Species_change/complete_species.csv")
