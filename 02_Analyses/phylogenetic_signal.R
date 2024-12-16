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


#delete unnecessary columns
um_win = subset(um_win, select = -c(X,originSite,treatment))

#get rid of unidentified carex
um_win <- um_win %>% filter(!species %in% "Carex_sp.")

#match order of species in trait data with order in phylogeny
abundance_df <- abundance_df[ order(match(abundance_df$Species, 
                                          specieslist$specieslist)), ]

#reformat
abundance_df <- abundance_df %>% remove_rownames %>% column_to_rownames(var="Species")
abundance_df <- df2vec(abundance_df, colID=1)

#save as csv
write.csv(abundance_df, file = "comm_phylo_analyses/Phylogenetic_signal/abundance_trait_data_new.csv")

#calculate signal
phylosignal(abundance_df, pruned.tree, reps = 5000, checkdata = TRUE) #with picante
phylosig(pruned.tree, abundance_df, method="K", test=TRUE, nsim=5000,
         se=NULL, start=NULL, control=list(), niter=10) #with phytools, gives same answer

###cooled 1#####

###cooled 2#####

##Origin: Pfeiler-------

##Origin: Monument-------

#Pagels lambda----------
