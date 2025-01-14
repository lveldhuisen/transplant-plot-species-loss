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

#Blomberg's K--------------

##Origin: Upper Montane-------
###within site#####
#make trait dataframe
um_win_sig <- read.csv("Data/Species_change/UM_win_slopes.csv")

#delete unnecessary columns
um_win_sig = subset(um_win_sig, select = -c(originSite,treatment))

#match order of species in trait data with order in phylogeny
um_win_sig <- um_win_sig[ order(match(um_win_sig$species, 
                                          specieslist$specieslist)), ]

#reformat
um_win_sig <- um_win_sig %>% remove_rownames %>% column_to_rownames(var="species")

#convert to vector
um_win_sig <- df2vec(um_win_sig, colID=1)

#save as csv
write.csv(um_win_sig, file = "Data/Species_change/UM_within_forsignal.csv")

#calculate signal
phylosignal(um_win_sig, pruned.tree, reps = 5000, checkdata = TRUE) #with picante

###cooled 1#####

#make trait dataframe
um_c1_sig <- read.csv("Data/Species_change/UM_c1_slopes.csv")

#delete unnecessary columns
um_c1_sig = subset(um_c1_sig, select = -c(originSite,treatment))

#get rid of species not in phylogeny
out.species <- c("Carex_sp.","Senecio_crassulus")
um_c1_sig <- um_c1_sig %>% filter(!species %in% out.species)

#match order of species in trait data with order in phylogeny
um_c1_sig <- um_c1_sig[ order(match(um_c1_sig$species, 
                                      specieslist$specieslist)), ]

#reformat
um_c1_sig <- um_c1_sig %>% remove_rownames %>% column_to_rownames(var="species")

#remove NAs
um_c1_sig <- na.omit(um_c1_sig)

#rename species to match phylogeny
row.names(um_c1_sig)[26] <- "Polygonum_douglasii_subsp._douglasii" 
row.names(um_c1_sig)[25] <- "Poa_pratensis_subsp._pratensis" 



#convert to vector
um_c1_sig <- df2vec(um_c1_sig, colID=1)

#save as csv
write.csv(um_c1_sig, file = "Data/Species_change/UM_c1_forsignal.csv")

#calculate signal
phylosignal(um_c1_sig, pruned.tree, reps = 5000, checkdata = TRUE) #with picante

###cooled 2#####
#make trait dataframe
um_c2_sig <- read.csv("Data/Species_change/UM_c2_slopes.csv")

#delete unnecessary columns
um_c2_sig = subset(um_c2_sig, select = -c(originSite,treatment))

#get rid of species not in phylogeny
out.species <- c("Carex_sp.","Lupinus_sp.")
um_c2_sig <- um_c2_sig %>% filter(!species %in% out.species)

#match order of species in trait data with order in phylogeny
um_c2_sig <- um_c2_sig[ order(match(um_c2_sig$species, 
                                    specieslist$specieslist)), ]

#reformat
um_c2_sig <- um_c2_sig %>% remove_rownames %>% column_to_rownames(var="species")

#remove NAs
um_c2_sig <- na.omit(um_c2_sig)

#rename species to match phylogeny
row.names(um_c2_sig)[25] <- "Polygonum_douglasii_subsp._douglasii" 
row.names(um_c2_sig)[24] <- "Poa_pratensis_subsp._pratensis" 
row.names(um_c2_sig)[22] <- "Helianthella_uniflora" 

#convert to vector
um_c2_sig <- df2vec(um_c2_sig, colID=1)

#save as csv
write.csv(um_c2_sig, file = "Data/Species_change/UM_c2_forsignal.csv")

#calculate signal
phylosignal(um_c2_sig, pruned.tree, reps = 5000, checkdata = TRUE) #with picante

##Origin: Pfeiler-------

###within site transplant####
#make trait dataframe
pf_win_sig <- read.csv("Data/Species_change/Pfeiler_win_slopes.csv")

#delete unnecessary columns
pf_win_sig = subset(pf_win_sig, select = -c(originSite,treatment))

#get rid of species not in phylogeny
out.species <- c("Carex_sp.","Lupinus_sp.", "Erigeron_sp.", "Senecio_crassulus")
pf_win_sig <- pf_win_sig %>% filter(!species %in% out.species)

#match order of species in trait data with order in phylogeny
pf_win_sig <- pf_win_sig[ order(match(pf_win_sig$species, 
                                    specieslist$specieslist)), ]

#reformat
pf_win_sig <- pf_win_sig %>% remove_rownames %>% column_to_rownames(var="species")

#remove NAs
pf_win_sig <- na.omit(pf_win_sig)

#rename species to match phylogeny
row.names(pf_win_sig)[24] <- "Polygonum_douglasii_subsp._douglasii" 
row.names(pf_win_sig)[20] <- "Agoseris_glauca_var._dasycephala" 
row.names(pf_win_sig)[22] <- "Helianthella_uniflora" 

#convert to vector
pf_win_sig <- df2vec(pf_win_sig, colID=1)

#save as csv
write.csv(pf_win_sig, file = "Data/Species_change/Pfeiler_win_forsignal.csv")

#calculate signal
phylosignal(pf_win_sig, pruned.tree, reps = 5000, checkdata = TRUE) #with picante


##Origin: Monument-------

