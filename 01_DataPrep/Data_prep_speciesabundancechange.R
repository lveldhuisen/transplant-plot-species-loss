library(dplyr)
library(tidyverse)
library(ggplot2)

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

#Fill abundance columns and calculate change across plots------

##Origin: Upper Montane####

###Tx: within site transplant#####
#split into separate dataframes for each year
um_win18 <- um_win %>% filter(year == "2018")
um_win23 <- um_win %>% filter(year == "2023")

#rename columns
colnames(um_win18)[colnames(um_win18) == 'occurrenceCount'] <- 'Ab2018'
colnames(um_win23)[colnames(um_win23) == 'occurrenceCount'] <- 'Ab2023'

#rejoin dataframes
um_win <- um_win18 %>% left_join(um_win23, 
                             by=c('turfID','species'))
#remove extra columns
um_win = subset(um_win, select = -c(originSite.y,
                                                  destinationSite.y,
                                                  originPlotID.y,
                                                  treatment.y,
                                    treatmentOriginGroup.y,
                                    year.y,
                                    AOO.y, 
                                    year.x))

#replace NAs with 0
um_win[is.na(um_win)] = 0 

#add column for change for each plot
um_win$delta <- (log(um_win$Ab2023+1)-log(um_win$Ab2018+1))

#average change for each species across plots
um_win$meandelta <- NA
um_win2 <- um_win %>% 
  group_by(species) %>% 
  summarise(meandelta = mean(delta))

#add column for origin/treatment
um_win2$originSite <- "Upper Montane"
um_win2$treatment <- "Within_site_transplant"

#save as csv
write.csv(um_win2, "Data/Species_change/UM_withinsite.csv")

###Tx: cooled 1#####
#split into separate dataframes for each year
um_c1_18 <- um_c1 %>% filter(year == "2018")
um_c1_23 <- um_c1 %>% filter(year == "2023")

#rename columns
colnames(um_c1_18)[colnames(um_c1_18) == 'occurrenceCount'] <- 'Ab2018'
colnames(um_c1_23)[colnames(um_c1_23) == 'occurrenceCount'] <- 'Ab2023'

#rejoin dataframes
um_c1 <- um_c1_18 %>% left_join(um_c1_23, 
                                 by=c('turfID','species'))
#remove extra columns
um_c1 = subset(um_c1, select = -c(originSite.y,
                                    destinationSite.y,
                                    originPlotID.y,
                                    treatment.y,
                                    treatmentOriginGroup.y,
                                    year.y,
                                    AOO.y, 
                                    year.x))

#replace NAs with 0
um_c1[is.na(um_c1)] = 0 

#add column for change for each plot
um_c1$delta <- (log(um_c1$Ab2023+1)-log(um_c1$Ab2018+1))

#average change for each species across plots
um_c1$meandelta <- NA
um_c1_2 <- um_c1 %>% 
  group_by(species) %>% 
  summarise(meandelta = mean(delta))

#add column for origin/treatment
um_c1_2$originSite <- "Upper Montane"
um_c1_2$treatment <- "Cooled_one_step"

#save as csv
write.csv(um_c1_2, "Data/Species_change/UM_cooled1.csv")

###Tx: cooled 2#####   
#split into separate dataframes for each year
um_c2_18 <- um_c2 %>% filter(year == "2018")
um_c2_23 <- um_c2 %>% filter(year == "2023")

#rename columns
colnames(um_c2_18)[colnames(um_c2_18) == 'occurrenceCount'] <- 'Ab2018'
colnames(um_c2_23)[colnames(um_c2_23) == 'occurrenceCount'] <- 'Ab2023'

#rejoin dataframes
um_c2 <- um_c2_18 %>% left_join(um_c2_23, 
                                by=c('turfID','species'))
#remove extra columns
um_c2 = subset(um_c2, select = -c(originSite.y,
                                  destinationSite.y,
                                  originPlotID.y,
                                  treatment.y,
                                  treatmentOriginGroup.y,
                                  year.y,
                                  AOO.y, 
                                  year.x))

#replace NAs with 0
um_c2[is.na(um_c2)] = 0 

#add column for change for each plot
um_c2$delta <- (log(um_c2$Ab2023+1)-log(um_c2$Ab2018+1))

#average change for each species across plots
um_c2$meandelta <- NA
um_c2_2 <- um_c2 %>% 
  group_by(species) %>% 
  summarise(meandelta = mean(delta))

#add column for origin/treatment
um_c2_2$originSite <- "Upper Montane"
um_c2_2$treatment <- "Cooled_two_steps"

#save as csv
write.csv(um_c2_2, "Data/Species_change/UM_cooled2.csv")

##Origin: Pfeiler####

###Tx: within site transplant#####
#split into separate dataframes for each year
pf_win_18 <- pf_win %>% filter(year == "2018")
pf_win_23 <- pf_win %>% filter(year == "2023")

#rename columns
colnames(pf_win_18)[colnames(pf_win_18) == 'occurrenceCount'] <- 'Ab2018'
colnames(pf_win_23)[colnames(pf_win_23) == 'occurrenceCount'] <- 'Ab2023'

#rejoin dataframes
pf_win <- pf_win_18 %>% left_join(pf_win_23, 
                                by=c('turfID','species'))
#remove extra columns
pf_win = subset(pf_win, select = -c(originSite.y,
                                  destinationSite.y,
                                  originPlotID.y,
                                  treatment.y,
                                  treatmentOriginGroup.y,
                                  year.y,
                                  AOO.y, 
                                  year.x))

#replace NAs with 0
pf_win[is.na(pf_win)] = 0 

#add column for change for each plot
pf_win$delta <- (log(pf_win$Ab2023+1)-log(pf_win$Ab2018+1))

#average change for each species across plots
pf_win$meandelta <- NA
pf_win2 <- pf_win %>% 
  group_by(species) %>% 
  summarise(meandelta = mean(delta))

#add column for origin/treatment
pf_win2$originSite <- "Pfeiler"
pf_win2$treatment <- "Within_site_transplant"

#save as csv
write.csv(pf_win2, "Data/Species_change/Pfeiler_withinsite.csv")

###Tx: warmed 1#####
#split into separate dataframes for each year
pf_w1_18 <- pf_w1 %>% filter(year == "2018")
pf_w1_23 <- pf_w1 %>% filter(year == "2023")

#rename columns
colnames(pf_w1_18)[colnames(pf_w1_18) == 'occurrenceCount'] <- 'Ab2018'
colnames(pf_w1_23)[colnames(pf_w1_23) == 'occurrenceCount'] <- 'Ab2023'

#rejoin dataframes
pf_w1 <- pf_w1_18 %>% left_join(pf_w1_23, 
                                  by=c('turfID','species'))
#remove extra columns
pf_w1 = subset(pf_w1, select = -c(originSite.y,
                                    destinationSite.y,
                                    originPlotID.y,
                                    treatment.y,
                                    treatmentOriginGroup.y,
                                    year.y,
                                    AOO.y, 
                                    year.x))

#replace NAs with 0
pf_w1[is.na(pf_w1)] = 0 

#add column pf_w1#add column for change for each plot
pf_w1$delta <- (log(pf_w1$Ab2023+1)-log(pf_w1$Ab2018+1))

#average change for each species across plots
pf_w1$meandelta <- NA
pf_w1_2 <- pf_w1 %>% 
  group_by(species) %>% 
  summarise(meandelta = mean(delta))

#add column for origin/treatment
pf_w1_2$originSite <- "Pfeiler"
pf_w1_2$treatment <- "Warmed_one_step"

#save as csv
write.csv(pf_w1_2, "Data/Species_change/Pfeiler_warmed1.csv")

###Tx: cooled 1#####
#split into separate dataframes for each year
pf_c1_18 <- pf_c1 %>% filter(year == "2018")
pf_c1_23 <- pf_c1 %>% filter(year == "2023")

#rename columns
colnames(pf_c1_18)[colnames(pf_c1_18) == 'occurrenceCount'] <- 'Ab2018'
colnames(pf_c1_23)[colnames(pf_c1_23) == 'occurrenceCount'] <- 'Ab2023'

#rejoin dataframes
pf_c1 <- pf_c1_18 %>% left_join(pf_c1_23, 
                                by=c('turfID','species'))
#remove extra columns
pf_c1 = subset(pf_c1, select = -c(originSite.y,
                                  destinationSite.y,
                                  originPlotID.y,
                                  treatment.y,
                                  treatmentOriginGroup.y,
                                  year.y,
                                  AOO.y, 
                                  year.x))

#replace NAs with 0
pf_c1[is.na(pf_c1)] = 0 

#add column pf_w1#add column for change for each plot
pf_c1$delta <- (log(pf_c1$Ab2023+1)-log(pf_c1$Ab2018+1))

#average change for each species across plots
pf_c1$meandelta <- NA
pf_c1_2 <- pf_c1 %>% 
  group_by(species) %>% 
  summarise(meandelta = mean(delta))

#add column for origin/treatment
pf_c1_2$originSite <- "Pfeiler"
pf_c1_2$treatment <- "Cooled_one_step"

#save as csv
write.csv(pf_c1_2, "Data/Species_change/Pfeiler_cooled1.csv")

##Origin: Monument########

###Tx: within site transplant######
#split into separate dataframes for each year
mo_win_18 <- mo_win %>% filter(year == "2018")
mo_win_23 <- mo_win %>% filter(year == "2023")

#rename columns
colnames(mo_win_18)[colnames(mo_win_18) == 'occurrenceCount'] <- 'Ab2018'
colnames(mo_win_23)[colnames(mo_win_23) == 'occurrenceCount'] <- 'Ab2023'

#rejoin dataframes
mo_win <- mo_win_18 %>% left_join(mo_win_23, 
                                by=c('turfID','species'))
#remove extra columns
mo_win = subset(mo_win, select = -c(originSite.y,
                                  destinationSite.y,
                                  originPlotID.y,
                                  treatment.y,
                                  treatmentOriginGroup.y,
                                  year.y,
                                  AOO.y, 
                                  year.x))

#replace NAs with 0
mo_win[is.na(mo_win)] = 0 

#add column pf_w1#add column for change for each plot
mo_win$delta <- (log(mo_win$Ab2023+1)-log(mo_win$Ab2018+1))

#average change for each species across plots
mo_win$meandelta <- NA
mo_win2 <- mo_win %>% 
  group_by(species) %>% 
  summarise(meandelta = mean(delta))

#add column for origin/treatment
mo_win2$originSite <- "Monument"
mo_win2$treatment <- "Within_site_transplant"

#save as csv
write.csv(mo_win2, "Data/Species_change/Monument_withinsite.csv")

###Tx: warmed 1#######
#split into separate dataframes for each year
mo_w1_18 <- mo_w1 %>% filter(year == "2018")
mo_w1_23 <- mo_w1 %>% filter(year == "2023")

#rename columns
colnames(mo_w1_18)[colnames(mo_w1_18) == 'occurrenceCount'] <- 'Ab2018'
colnames(mo_w1_23)[colnames(mo_w1_23) == 'occurrenceCount'] <- 'Ab2023'

#rejoin dataframes
mo_w1 <- mo_w1_18 %>% left_join(mo_w1_23, 
                                  by=c('turfID','species'))
#remove extra columns
mo_w1 = subset(mo_w1, select = -c(originSite.y,
                                    destinationSite.y,
                                    originPlotID.y,
                                    treatment.y,
                                    treatmentOriginGroup.y,
                                    year.y,
                                    AOO.y, 
                                    year.x))

#replace NAs with 0
mo_w1[is.na(mo_w1)] = 0 

#add column pf_w1#add column for change for each plot
mo_w1$delta <- (log(mo_w1$Ab2023+1)-log(mo_w1$Ab2018+1))

#average change for each species across plots
mo_w1$meandelta <- NA
mo_w1_2 <- mo_w1 %>% 
  group_by(species) %>% 
  summarise(meandelta = mean(delta))

#add column for origin/treatment
mo_w1_2$originSite <- "Monument"
mo_w1_2$treatment <- "Warmed_one_step"

#save as csv
write.csv(mo_w1_2, "Data/Species_change/Monument_warmed1.csv")

###Tx: warmed 2#####
#split into separate dataframes for each year
mo_w2_18 <- mo_w2 %>% filter(year == "2018")
mo_w2_23 <- mo_w2 %>% filter(year == "2023")

#rename columns
colnames(mo_w2_18)[colnames(mo_w2_18) == 'occurrenceCount'] <- 'Ab2018'
colnames(mo_w2_23)[colnames(mo_w2_23) == 'occurrenceCount'] <- 'Ab2023'

#rejoin dataframes
mo_w2 <- mo_w2_18 %>% left_join(mo_w2_23, 
                                by=c('turfID','species'))
#remove extra columns
mo_w2 = subset(mo_w2, select = -c(originSite.y,
                                  destinationSite.y,
                                  originPlotID.y,
                                  treatment.y,
                                  treatmentOriginGroup.y,
                                  year.y,
                                  AOO.y, 
                                  year.x))

#replace NAs with 0
mo_w2[is.na(mo_w2)] = 0 

#add column pf_w1#add column for change for each plot
mo_w2$delta <- (log(mo_w2$Ab2023+1)-log(mo_w2$Ab2018+1))

#average change for each species across plots
mo_w2$meandelta <- NA
mo_w2_2 <- mo_w2 %>% 
  group_by(species) %>% 
  summarise(meandelta = mean(delta))

#add column for origin/treatment
mo_w2_2$originSite <- "Monument"
mo_w2_2$treatment <- "Warmed_two_steps"

#save as csv
write.csv(mo_w2_2, "Data/Species_change/Monument_warmed2.csv")

#Merge all species change into one big dataframe---------

df_all <- bind_rows(um_win2, um_c1_2, um_c2_2, pf_win2, pf_w1_2, pf_c1_2,
                mo_win2, mo_w1_2, mo_w2_2)

#save as csv
write.csv(df_all, "Data/Species_change/species_change_all.csv")
