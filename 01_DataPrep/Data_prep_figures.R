library(tidyverse)
library(ggeffects)
library(sjPlot)
library(dplyr)

#Richness--------------
#bring in data
pred_R <- read.csv("ModelOutput/Prediction_richness_nested.csv")

#remove weird site comparisons
site_ins <- c("Monument-Monument","Pfeiler-Pfeiler","Upper Montane-Upper Montane")

pred_R <- pred_R %>% filter(originSite %in% site_ins,
                            !is.na(conf.low))

#split treatment column
pred_R <- separate_wider_delim(pred_R, cols = treatment, delim = "-", 
                               names = c("comparison1", "comparison2"))

#keep only treatments compared to within site
pred_R <- pred_R %>% filter(comparison1 %in% "within_site_transplant")

#multiply by neg 1
pred_R$Contrast <- pred_R$Contrast*-1
pred_R$conf.low <- pred_R$conf.low*-1
pred_R$conf.high <- pred_R$conf.high*-1

#redo site name values
pred_R$originSite[pred_R$originSite=="Monument-Monument"]<-"Monument"
pred_R$originSite[pred_R$originSite=="Pfeiler-Pfeiler"]<-"Pfeiler"
pred_R$originSite[pred_R$originSite=="Upper Montane-Upper Montane"]<-"Upper Montane"

#Shannon------------


#PD--------

#MPD----------

#MNTD---------