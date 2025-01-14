library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(plyr)
library(picante)
library(geiger)
library(ape)
library(phytools)

#Combine all treatment/origin site datasets into one------

#bring in data
um_win_slopes <- read.csv("Data/Species_change/UM_win_slopes.csv")
um_c1_slopes <- read.csv("Data/Species_change/UM_c1_slopes.csv")
um_c2_slopes <- read.csv("Data/Species_change/UM_c2_slopes.csv")

pf_win_slopes <- read.csv("Data/Species_change/Pfeiler_win_slopes.csv")
pf_c1_slopes <- read.csv("Data/Species_change/Pfeiler_c1_slopes.csv")
pf_w1_slopes <- read.csv("Data/Species_change/Pfeiler_w1_slopes.csv")

mo_win_slopes <- read.csv("Data/Species_change/Monument_within_slopes.csv")
mo_w1_slopes <- read.csv("Data/Species_change/Monument_w1_slopes.csv")
mo_w2_slopes <- read.csv("Data/Species_change/Monument_w2_slopes.csv")

#combine datasets
um_all <- bind_rows(um_win_slopes, um_c1_slopes, um_c2_slopes)
pf_all <- bind_rows(pf_win_slopes, pf_c1_slopes, pf_w1_slopes)
mo_all <- bind_rows(mo_win_slopes, mo_w1_slopes, mo_w2_slopes)

slopes_df <- bind_rows(um_all, pf_all, mo_all)

##remove NAs (species with an occurrence in only one year)#####
slopes_df <- na.omit(slopes_df)

##replace species names with phylogeny substitutes in table sx#####
slopes_df$species[slopes_df$species == 'Agoseris_glauca'] <- 'Agoseris_glauca_var._dasycephala'
slopes_df$species[slopes_df$species == 'Aquilegia_caerulea'] <- 'Aquilegia_coerulea'
slopes_df$species[slopes_df$species == 'Epilobium_sp.'] <- 'Epilobium_ciliatum'
slopes_df$species[slopes_df$species == 'Erigeron_elatior'] <- 'Erigeron_grandiflorus'
slopes_df$species[slopes_df$species == 'Festuca_rubra'] <- 'Festuca_rubra_subsp._rubra'
slopes_df$species[slopes_df$species == 'Helianthella_quinquenervis'] <- 'Helianthella_uniflora'
slopes_df$species[slopes_df$species == 'Heterotheca_pumila'] <- 'Heterotheca_villosa'
slopes_df$species[slopes_df$species == 'Hydrophyllum_capitatum'] <- 'Hydrophyllum_capitatum_var._capitatum'
slopes_df$species[slopes_df$species == 'Lupinus_sp.'] <- 'Lupinus_argenteus'
slopes_df$species[slopes_df$species == 'Poa_pratensis'] <- 'Poa_pratensis_subsp._pratensis'
slopes_df$species[slopes_df$species == 'Polygonum_douglasii'] <- 'Polygonum_douglasii_subsp._douglasii'
slopes_df$species[slopes_df$species == 'Senecio_integerrimus'] <- 'Senecio_triangularis'
slopes_df$species[slopes_df$species == 'Symphyotrichum_ascendens'] <- 'Symphyotrichum_foliaceum'
slopes_df$species[slopes_df$species == 'Carex_sp.'] <- 'Carex_nelsonii'

##get rid of dropped species####
species.outs <- c("Erigeron_sp.","Erigeron_coulteri",
             "Senecio_crassulus")
slopes_df <- slopes_df %>% filter(!species %in% species.outs)

#break back out into 9 treatment x origin site groups---------

um_win_signal <- slopes_df %>% filter(originSite == "Upper Montane",
                                   treatment == "within_site_transplant")

um_c1_signal <- slopes_df %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_one")

um_c2_signal <- slopes_df %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_two")

pf_win_signal <- slopes_df %>% filter(originSite == "Pfeiler",
                                   treatment == "within_site_transplant")

pf_w1_signal <- slopes_df %>% filter(originSite == "Pfeiler",
                                  treatment == "warmed_one")

pf_c1_signal <- slopes_df %>% filter(originSite == "Pfeiler",
                                  treatment == "cooled_one")

mo_win_signal <- slopes_df %>% filter(originSite == "Monument",
                                   treatment == "within_site_transplant")

mo_w1_signal <- slopes_df %>% filter(originSite == "Monument",
                                  treatment == "warmed_one")

mo_w2_signal <- slopes_df %>% filter(originSite == "Monument",
                                  treatment == "warmed_two")

#format each slope dataframe for phylogenetic signal calculation-------------
