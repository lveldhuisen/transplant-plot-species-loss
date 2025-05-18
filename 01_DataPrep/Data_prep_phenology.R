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

slopes_pfeiler <- slopes_df %>% filter(originSite %in% "Pfeiler")
pheno_pfeiler <- pheno_df %>% filter(Site %in% "Pfeiler")

#delete extra columns
pheno_pfeiler = subset(pheno_pfeiler, select = -c(X,
                                                   X.1,
                                                  X.2,
                                                  X.3,
                                                  X.4,
                                                  X.5))

slopes_pfeiler = subset(slopes_pfeiler, select = -X)  

#update species names
pheno_pfeiler$Species[pheno_pfeiler$Species == 'Agoseris_glauca'] <- 'Agoseris_glauca_var._dasycephala'
pheno_pfeiler$Species[pheno_pfeiler$Species == 'Helianthella_quinquenervis'] <- 'Helianthella_uniflora'
pheno_pfeiler$Species[pheno_pfeiler$Species == 'Heterotheca_pumila'] <- 'Heterotheca_villosa'
pheno_pfeiler$Species[pheno_pfeiler$Species == 'Hydrophyllum_capitatum'] <- 'Hydrophyllum_capitatum_var._capitatum'
pheno_pfeiler$Species[pheno_pfeiler$Species == 'Senecio_integerrimus'] <- 'Senecio_triangularis'

#rename species column to match 
colnames(pheno_pfeiler)[colnames(pheno_pfeiler) == 'Species'] <- 'species'

#combine datasets
test <- left_join(slopes_pfeiler, pheno_pfeiler, by=c("species"), relationship = "many-to-many")

#bring in other data
test_df <- read.csv("Data/Species_change/Pfeiler_pheno_slopes.csv")


#test differences in slopes between modules

ggplot(test_df, aes(x= flowering_module, y=slope))+
  geom_boxplot()+
  theme_bw()
