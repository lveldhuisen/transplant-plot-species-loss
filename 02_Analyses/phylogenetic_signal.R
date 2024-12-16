library(tidyverse)
library(dplyr)
library(picante)
library(geiger)
library(ape)
library(phytools)
library(viridis)
library(viridisLite)
library(patchwork)

#import S&B phylogeny--------------------

#bring in .tre file
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL phylogeny/Smith&Brown18")
SBtree <- read.tree(file = "ALLMB.tre")
is.rooted(SBtree)


#check for correct species in pruned tree
specieslist <- pruned.tree$tip.label
specieslist <- as.data.frame(specieslist)

#format trait data to match phylogeny-------
change_df <- read.csv("Data/Species_change/species_change_all.csv")

#Blomberg's K--------------

##Origin: Upper Montane-------
###within site#####

#make trait dataframe
um_win_sig <- read.csv("Data/Species_change/UM_withinsite.csv")

#delete unnecessary columns
um_win_sig = subset(um_win_sig, select = -c(X,originSite,treatment))

#get rid of unidentified carex
um_win_sig <- um_win_sig %>% filter(!species %in% "Carex_sp.")

#match order of species in trait data with order in phylogeny
um_win_sig <- um_win_sig[ order(match(um_win_sig$species, 
                                          specieslist$specieslist)), ]

#reformat
um_win_sig <- um_win_sig %>% remove_rownames %>% column_to_rownames(var="species")

#rename species to match phylogeny
row.names(um_win_sig)[26] <- "Helianthella_uniflora" 
row.names(um_win_sig)[28] <- "Poa_pratensis_subsp._pratensis" 

#convert to vector
um_win_sig <- df2vec(um_win_sig, colID=1)

#save as csv
write.csv(um_win_sig, file = "Data/Species_change/UM_within_forsignal.csv")

#calculate signal
phylosignal(um_win_sig, pruned.tree, reps = 5000, checkdata = TRUE) #with picante

###cooled 1#####

###cooled 2#####

##Origin: Pfeiler-------

##Origin: Monument-------

#Pagels lambda----------
