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
slopes_df$species[slopes_df$species == 'Senecio_integerrimus'] <- 'Senecio_integerrimus_var._exaltatus'
slopes_df$species[slopes_df$species == 'Symphyotrichum_ascendens'] <- 'Symphyotrichum_foliaceum'
slopes_df$species[slopes_df$species == 'Carex_sp.'] <- 'Carex_nelsonii'

##get rid of dropped species####
species.outs <- c("Erigeron_sp.","Erigeron_coulteri",
             "Senecio_crassulus")
slopes_df <- slopes_df %>% filter(!species %in% species.outs)

write.csv(slopes_df, "Data/Species_change/Cover_slopes_all.csv")

#break back out into 9 treatment x origin site groups---------

um_win_sig <- slopes_df %>% filter(originSite == "Upper Montane",
                                   treatment == "within_site_transplant")

um_c1_sig <- slopes_df %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_one")

um_c2_sig <- slopes_df %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_two")

pf_win_sig<- slopes_df %>% filter(originSite == "Pfeiler",
                                   treatment == "within_site_transplant")

pf_w1_sig <- slopes_df %>% filter(originSite == "Pfeiler",
                                  treatment == "warmed_one")

pf_c1_sig<- slopes_df %>% filter(originSite == "Pfeiler",
                                  treatment == "cooled_one")

mo_win_sig <- slopes_df %>% filter(originSite == "Monument",
                                   treatment == "within_site_transplant")

mo_w1_sig <- slopes_df %>% filter(originSite == "Monument",
                                  treatment == "warmed_one")

mo_w2_sig <- slopes_df %>% filter(originSite == "Monument",
                                  treatment == "warmed_two")

#format each slope dataframe for phylogenetic signal calculation-------------

##Upper Montane#####
###within#####
#delete unnecessary columns
um_win_sig = subset(um_win_sig, select = -c(originSite,treatment))

#match order of species in trait data with order in phylogeny
um_win_sig <- um_win_sig[ order(match(um_win_sig$species, 
                                      specieslist$specieslist)), ]

#make extra to turn into phylogeny
um_win_forphylo <- um_win_sig
um_win_forphylo$slope <- 1

#reformat
um_win_sig <- um_win_sig %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
um_win_sig <- df2vec(um_win_sig, colID=1)

#save as csv
write.csv(um_win_sig, file = "Data/Species_change/UM_within_forsignal.csv")

###cooled one####
#delete unnecessary columns
um_c1_sig = subset(um_c1_sig, select = -c(originSite,treatment))

#match order of species in trait data with order in phylogeny
um_c1_sig <- um_c1_sig[ order(match(um_c1_sig$species, 
                                    specieslist$specieslist)), ]

#reformat
um_c1_sig <- um_c1_sig %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
um_c1_sig <- df2vec(um_c1_sig, colID=1)

#save as csv
write.csv(um_c1_sig, file = "Data/Species_change/UM_c1_forsignal.csv")

###cooled two#####

#delete unnecessary columns
um_c2_sig = subset(um_c2_sig, select = -c(originSite,treatment))

#match order of species in trait data with order in phylogeny
um_c2_sig <- um_c2_sig[ order(match(um_c2_sig$species, 
                                    specieslist$specieslist)), ]

#reformat
um_c2_sig <- um_c2_sig %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
um_c2_sig <- df2vec(um_c2_sig, colID=1)

#save as csv
write.csv(um_c2_sig, file = "Data/Species_change/UM_c2_forsignal.csv")

##Pfeiler#####

###within#####
#delete unnecessary columns
pf_win_sig = subset(pf_win_sig, select = -c(originSite,treatment))

#match order of species in trait data with order in phylogeny
pf_win_sig <- pf_win_sig[ order(match(pf_win_sig$species, 
                                      specieslist$specieslist)), ]

#reformat
pf_win_sig <- pf_win_sig %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
pf_win_sig <- df2vec(pf_win_sig, colID=1)

#save as csv
write.csv(pf_win_sig, file = "Data/Species_change/Pfeiler_win_forsignal.csv")

###cooled one####

#delete unnecessary columns
pf_c1_sig = subset(pf_c1_sig, select = -c(originSite,treatment))

#match order of species in trait data with order in phylogeny
pf_c1_sig <- pf_c1_sig[ order(match(pf_c1_sig$species, 
                                      specieslist$specieslist)), ]

#reformat
pf_c1_sig <- pf_c1_sig %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
pf_c1_sig <- df2vec(pf_c1_sig, colID=1)

#save as csv
write.csv(pf_c1_sig, file = "Data/Species_change/Pfeiler_c1_forsignal.csv")

###warmed one####
#delete unnecessary columns
pf_w1_sig = subset(pf_w1_sig, select = -c(originSite,treatment))

#match order of species in trait data with order in phylogeny
pf_w1_sig <- pf_w1_sig[ order(match(pf_w1_sig$species, 
                                    specieslist$specieslist)), ]

#reformat
pf_w1_sig <- pf_w1_sig %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
pf_w1_sig <- df2vec(pf_w1_sig, colID=1)

#save as csv
write.csv(pf_w1_sig, file = "Data/Species_change/Pfeiler_w1_forsignal.csv")

##Monument####

###within####
#delete unnecessary columns
mo_win_sig = subset(mo_win_sig, select = -c(originSite,treatment))

#match order of species in trait data with order in phylogeny
mo_win_sig <- mo_win_sig[ order(match(mo_win_sig$species, 
                                    specieslist$specieslist)), ]

#reformat
mo_win_sig <- mo_win_sig %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
mo_win_sig <- df2vec(mo_win_sig, colID=1)

#save as csv
write.csv(mo_win_sig, file = "Data/Species_change/Monument_win_forsignal.csv")

###warmed one#####
#delete unnecessary columns
mo_w1_sig = subset(mo_w1_sig, select = -c(originSite,treatment))

#match order of species in trait data with order in phylogeny
mo_w1_sig <- mo_w1_sig[ order(match(mo_w1_sig$species, 
                                      specieslist$specieslist)), ]

#reformat
mo_w1_sig <- mo_w1_sig %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
mo_w1_sig <- df2vec(mo_w1_sig, colID=1)

#save as csv
write.csv(mo_w1_sig, file = "Data/Species_change/Monument_w1_forsignal.csv")

###warmed two######
#delete unnecessary columns
mo_w2_sig = subset(mo_w2_sig, select = -c(originSite,treatment))

#match order of species in trait data with order in phylogeny
mo_w2_sig <- mo_w2_sig[ order(match(mo_w2_sig$species, 
                                    specieslist$specieslist)), ]

#reformat
mo_w2_sig <- mo_w2_sig %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
mo_w2_sig <- df2vec(mo_w2_sig, colID=1)

#save as csv
write.csv(mo_w2_sig, file = "Data/Species_change/Monument_w2_forsignal.csv")

#Prep data for phylogeny visualization-----------

#make community data matrix for pruned phylogeny
um_win_matrix <- pivot_wider(um_win_forphylo, names_from = species, 
                           values_from = slope)

row.names(um_win_matrix)[1] <- "UM_WIN_all" 

#prune tree
tree.umwin <- prune.sample(um_win_matrix, SBtree)

plot(tree.umwin)
is.rooted(tree.umwin)

#reformat dataframe
um_win_sig <- as.data.frame(um_win_sig)
um_win_sig <- um_win_sig %>% remove_rownames %>% column_to_rownames(var="species")
um_win_forfig <- df2vec(um_win_sig, colID=1)

#make figure
phylo_um_win <- contMap(tree.umwin, um_win_forfig, res=100, plot=FALSE)
phylo_um_win <- setMap(phylo_um_win, viridisLite::viridis(n=8))
plot(phylo_um_win)
