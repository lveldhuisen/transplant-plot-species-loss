library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(ggpmisc)
library(plyr)


#bring in data

pheno_df <- read.csv("Data/flowering_modules.csv")
slopes_df <- read.csv("Data/Species_change/Cover_slopes_all.csv")

#filter for pfeiler only

slopes_pfeiler <- slopes_df %>% filter(originSite %in% "Low elevation (2900 m)")
pheno_pfeiler <- pheno_df %>% filter(Site %in% "Pfeiler")

#delete extra columns
pheno_pfeiler = subset(pheno_pfeiler, select = -c(X,
                                                   X.1,
                                                  X.2,
                                                  X.3,
                                                  X.4,
                                                  X.5))

slopes_pfeiler = subset(slopes_pfeiler, select = -X)                       
