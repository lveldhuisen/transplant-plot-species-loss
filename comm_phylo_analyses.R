#quantifying change in phylogenetic diversity from 2018-2023 in turf plots

library(tidyverse)
library(dplyr)
library(picante)
library(geiger)
library(ape)
library(vegan)
library(forcats)
library(broom)
library(janitor)
library(patchwork)
library(car)

#import S&B phylogeny-----------------------------------------------------------
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL phylogeny/Smith&Brown18")

##import S&B18 tree and check data## 
SBtree <- read.tree(file = "ALLMB.tre")
write.tree(SBtree)
is.rooted(SBtree)

##prune tree to include all species from all treatments and control groups, including 2017##
#bring in raw data
abundance_df <- read.csv("occurance2017-2023.csv")

# remove non-species from species column
gc.outs <- c("litter", "bare_soil", "rock")

#filter data for things you never want
abundance_forphylogeny <- abundance_df %>% filter(!is.na(treatment),
                                         !species %in% gc.outs)

#prune tree
pruned.tree <- treedata(SBtree, unlist(community_matrix[4,community_matrix[4,]>0]), warnings = F)$phy
plot(pruned.tree)
