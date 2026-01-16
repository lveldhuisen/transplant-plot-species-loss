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
um_win_slopes_icv <- read.csv("Data/Species_change/UM_win_slopesICV.csv")
um_c1_slopes_icv <- read.csv("Data/Species_change/UM_c1_slopes_icv.csv")
um_c2_slopes_icv <- read.csv("Data/Species_change/UM_c2_slopes_icv.csv")

pf_win_slopes_icv <- read.csv("Data/Species_change/Pfeiler_win_slopes_icv.csv")
pf_c1_slopes_icv <- read.csv("Data/Species_change/Pfeiler_c1_slopes_icv.csv")
pf_w1_slopes_icv <- read.csv("Data/Species_change/Pfeiler_w1_slopes_icv.csv")

mo_win_slopes_icv <- read.csv("Data/Species_change/Monument_within_slopes_icv.csv")
mo_w1_slopes_icv <- read.csv("Data/Species_change/Monument_w1_slopes_icv.csv")
mo_w2_slopes_icv <- read.csv("Data/Species_change/Monument_w2_slopes_icv.csv")

#combine datasets
um_all_icv <- bind_rows(um_win_slopes_icv, um_c1_slopes_icv, um_c2_slopes_icv)
pf_all_icv <- bind_rows(pf_win_slopes_icv, pf_c1_slopes_icv, pf_w1_slopes_icv)
mo_all_icv <- bind_rows(mo_win_slopes_icv, mo_w1_slopes_icv, mo_w2_slopes_icv)

icv_df <- bind_rows(um_all_icv, pf_all_icv, mo_all_icv)

##remove NAs (species with an occurrence in only one year)#####
icv_df <- na.omit(icv_df)

##replace species names with phylogeny substitutes in table sx#####
icv_df$species[icv_df$species == 'Agoseris_glauca'] <- 'Agoseris_glauca_var._dasycephala'
icv_df$species[icv_df$species == 'Aquilegia_caerulea'] <- 'Aquilegia_coerulea'
icv_df$species[icv_df$species == 'Epilobium_sp.'] <- 'Epilobium_ciliatum'
icv_df$species[icv_df$species == 'Erigeron_elatior'] <- 'Erigeron_grandiflorus'
icv_df$species[icv_df$species == 'Festuca_rubra'] <- 'Festuca_rubra_subsp._rubra'
icv_df$species[icv_df$species == 'Helianthella_quinquenervis'] <- 'Helianthella_uniflora'
icv_df$species[icv_df$species == 'Heterotheca_pumila'] <- 'Heterotheca_villosa'
icv_df$species[icv_df$species == 'Hydrophyllum_capitatum'] <- 'Hydrophyllum_capitatum_var._capitatum'
icv_df$species[icv_df$species == 'Lupinus_sp.'] <- 'Lupinus_argenteus'
icv_df$species[icv_df$species == 'Poa_pratensis'] <- 'Poa_pratensis_subsp._pratensis'
icv_df$species[icv_df$species == 'Polygonum_douglasii'] <- 'Polygonum_douglasii_subsp._douglasii'
icv_df$species[icv_df$species == 'Senecio_integerrimus'] <- 'Senecio_integerrimus_var._exaltatus'
icv_df$species[icv_df$species == 'Symphyotrichum_ascendens'] <- 'Symphyotrichum_foliaceum'
icv_df$species[icv_df$species == 'Carex_sp.'] <- 'Carex_nelsonii'

##get rid of dropped species####
species.outs <- c("Erigeron_sp.","Erigeron_coulteri",
                  "Senecio_crassulus")
icv_df <- icv_df %>% filter(!species %in% species.outs)

write.csv(icv_df, "Data/Species_change/icv_forsignal.csv")

#break back out into 9 treatment x origin site groups---------

um_win_sig_icv <- icv_df %>% filter(originSite == "Upper Montane",
                                   treatment == "within_site_transplant")

um_c1_sig_icv <- icv_df %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_one")

um_c2_sig_icv <- icv_df %>% filter(originSite == "Upper Montane",
                                  treatment == "cooled_two")

pf_win_sig_icv <- icv_df %>% filter(originSite == "Pfeiler",
                                  treatment == "within_site_transplant")

pf_w1_sig_icv <- icv_df %>% filter(originSite == "Pfeiler",
                                  treatment == "warmed_one")

pf_c1_sig_icv <- icv_df %>% filter(originSite == "Pfeiler",
                                 treatment == "cooled_one")

mo_win_sig_icv <- icv_df %>% filter(originSite == "Monument",
                                   treatment == "within_site_transplant")

mo_w1_sig_icv <- icv_df %>% filter(originSite == "Monument",
                                  treatment == "warmed_one")

mo_w2_sig_icv <- icv_df %>% filter(originSite == "Monument",
                                  treatment == "warmed_two")

#format each slope dataframe for phylogenetic signal calculation-------------

##Upper Montane#####
###within#####
#delete unnecessary columns
um_win_sig_icv = subset(um_win_sig_icv, select = -c(originSite,treatment, slope, slope_se))

#match order of species in trait data with order in phylogeny
um_win_sig_icv <- um_win_sig_icv[ order(match(um_win_sig_icv$species, 
                                      specieslist$specieslist)), ]

#make extra to turn into phylogeny
um_win_forphylo <- um_win_sig_icv
um_win_forphylo$slope <- 1

#reformat
um_win_sig_icv <- um_win_sig_icv %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
um_win_sig_icv <- df2vec(um_win_sig_icv, colID=1)

#save as csv
write.csv(um_win_sig_icv, file = "Data/Species_change/UM_within_forsignal_icv.csv")

###cooled one####
#delete unnecessary columns
um_c1_sig_icv = subset(um_c1_sig_icv, select = -c(originSite,treatment, slope, slope_se))

#match order of species in trait data with order in phylogeny
um_c1_sig_icv <- um_c1_sig_icv[ order(match(um_c1_sig_icv$species, 
                                    specieslist$specieslist)), ]

#reformat
um_c1_sig_icv <- um_c1_sig_icv %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
um_c1_sig_icv <- df2vec(um_c1_sig_icv, colID=1)

#save as csv
write.csv(um_c1_sig_icv, file = "Data/Species_change/UM_c1_forsignal_icv.csv")

###cooled two#####

#delete unnecessary columns
um_c2_sig_icv = subset(um_c2_sig_icv, select = -c(originSite,treatment, slope, slope_se))

#match order of species in trait data with order in phylogeny
um_c2_sig_icv <- um_c2_sig_icv[ order(match(um_c2_sig_icv$species, 
                                    specieslist$specieslist)), ]

#reformat
um_c2_sig_icv <- um_c2_sig_icv %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
um_c2_sig_icv <- df2vec(um_c2_sig_icv, colID=1)

#save as csv
write.csv(um_c2_sig_icv, file = "Data/Species_change/UM_c2_forsignal_icv.csv")

##Pfeiler#####

###within#####
#delete unnecessary columns
pf_win_sig_icv = subset(pf_win_sig_icv, select = -c(originSite,treatment, slope, slope_se))

#match order of species in trait data with order in phylogeny
pf_win_sig_icv <- pf_win_sig_icv[ order(match(pf_win_sig_icv$species, 
                                      specieslist$specieslist)), ]

#reformat
pf_win_sig_icv <- pf_win_sig_icv %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
pf_win_sig_icv <- df2vec(pf_win_sig_icv, colID=1)

#save as csv
write.csv(pf_win_sig_icv, file = "Data/Species_change/Pfeiler_win_forsignal_icv.csv")

###cooled one####

#delete unnecessary columns
pf_c1_sig_icv = subset(pf_c1_sig_icv, select = -c(originSite,treatment, slope, slope_se))

#match order of species in trait data with order in phylogeny
pf_c1_sig_icv <- pf_c1_sig_icv[ order(match(pf_c1_sig_icv$species, 
                                    specieslist$specieslist)), ]

#reformat
pf_c1_sig_icv <- pf_c1_sig_icv %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
pf_c1_sig_icv <- df2vec(pf_c1_sig_icv, colID=1)

#save as csv
write.csv(pf_c1_sig_icv, file = "Data/Species_change/Pfeiler_c1_forsignal_icv.csv")

###warmed one####
#delete unnecessary columns
pf_w1_sig_icv = subset(pf_w1_sig_icv, select = -c(originSite,treatment, slope, slope_se))

#match order of species in trait data with order in phylogeny
pf_w1_sig_icv <- pf_w1_sig_icv[ order(match(pf_w1_sig_icv$species, 
                                    specieslist$specieslist)), ]

#reformat
pf_w1_sig_icv <- pf_w1_sig_icv %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
pf_w1_sig_icv <- df2vec(pf_w1_sig_icv, colID=1)

#save as csv
write.csv(pf_w1_sig_icv, file = "Data/Species_change/Pfeiler_w1_forsignal_icv.csv")

##Monument####

###within####
#delete unnecessary columns
mo_win_sig_icv = subset(mo_win_sig_icv, select = -c(originSite,treatment, slope, slope_se))

#match order of species in trait data with order in phylogeny
mo_win_sig_icv <- mo_win_sig_icv[ order(match(mo_win_sig_icv$species, 
                                      specieslist$specieslist)), ]

#reformat
mo_win_sig_icv <- mo_win_sig_icv %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
mo_win_sig_icv <- df2vec(mo_win_sig_icv, colID=1)

#save as csv
write.csv(mo_win_sig_icv, file = "Data/Species_change/Monument_win_forsignal_icv.csv")

###warmed one#####
#delete unnecessary columns
mo_w1_sig_icv = subset(mo_w1_sig_icv, select = -c(originSite,treatment, slope, slope_se))

#match order of species in trait data with order in phylogeny
mo_w1_sig_icv <- mo_w1_sig_icv[ order(match(mo_w1_sig_icv$species, 
                                    specieslist$specieslist)), ]

#reformat
mo_w1_sig_icv <- mo_w1_sig_icv %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
mo_w1_sig_icv <- df2vec(mo_w1_sig_icv, colID=1)

#save as csv
write.csv(mo_w1_sig_icv, file = "Data/Species_change/Monument_w1_forsignal_icv.csv")

###warmed two######
#delete unnecessary columns
mo_w2_sig_icv = subset(mo_w2_sig_icv, select = -c(originSite,treatment, slope, slope_se))

#match order of species in trait data with order in phylogeny
mo_w2_sig_icv <- mo_w2_sig_icv[ order(match(mo_w2_sig_icv$species, 
                                    specieslist$specieslist)), ]

#reformat
mo_w2_sig_icv <- mo_w2_sig_icv %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
mo_w2_sig_icv <- df2vec(mo_w2_sig_icv, colID=1)

#save as csv
write.csv(mo_w2_sig_icv, file = "Data/Species_change/Monument_w2_forsignal_icv.csv")

#Prep data for phylogeny visualization-----------

#make community data matrix for pruned phylogeny
um_c2_matrix <- pivot_wider(um_c2_sig, names_from = species, 
                            values_from = um_c2_sig)

row.names(um_c2_matrix)[1] <- "UM_C2_all" 

#prune tree
tree.umc2 <- prune.sample(um_c2_matrix, SBtree)

plot(tree.umc2)
is.rooted(tree.umc2)

Ntip(tree.umc2)
nrow(um_c2_sig)

#reformat dataframe
um_c2_sig <- as.data.frame(um_c2_sig)
um_c2_sig$species <- rownames(um_c2_sig)
um_c2_forfig <- df2vec(um_c2_sig, colID=1)

#make figure
phylo_um_c2<- contMap(tree.umc2, um_c2_forfig, res=100, plot=FALSE)
phylo_um_c2 <- setMap(phylo_um_c2, viridisLite::viridis(n=8))
plot(phylo_um_c2)
