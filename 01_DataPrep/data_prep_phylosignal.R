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

#make trait datasets for each treatment/origin group community--------

#bring in dataset
abundance_df1 <- read.csv("Data/abundance_clean2018-2023.csv")

#make 

#import S&B phylogeny--------------------

#bring in .tre file
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL phylogeny/Smith&Brown18")
SBtree <- read.tree(file = "ALLMB.tre")
write.tree(SBtree)
is.rooted(SBtree)

#import community data matrix
