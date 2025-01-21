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
#bring in data
pred_s <- read.csv("ModelOutput/Prediction_Shannon_nested.csv")

#remove weird site comparisons
site_ins <- c("Monument-Monument","Pfeiler-Pfeiler","Upper Montane-Upper Montane")

pred_s <- pred_s %>% filter(originSite %in% site_ins,
                            !is.na(conf.low))

#split treatment column
pred_s <- separate_wider_delim(pred_s, cols = treatment, delim = "-", 
                               names = c("comparison1", "comparison2"))

#keep only treatments compared to within site
pred_s <- pred_s %>% filter(comparison1 %in% "within_site_transplant")

#multiply by neg 1
pred_s$Contrast <- pred_s$Contrast*-1
pred_s$conf.low <- pred_s$conf.low*-1
pred_s$conf.high <- pred_s$conf.high*-1

#redo site name values
pred_s$originSite[pred_s$originSite=="Monument-Monument"]<-"Monument"
pred_s$originSite[pred_s$originSite=="Pfeiler-Pfeiler"]<-"Pfeiler"
pred_s$originSite[pred_s$originSite=="Upper Montane-Upper Montane"]<-"Upper Montane"

#save as csv
write_csv(pred_s, file = "Data/Shannon_model_forfig.csv")

#PD--------
#bring in data
pred_pd <- read.csv("ModelOutput/Prediction_pd_nested.csv")

#remove weird site comparisons
site_ins <- c("Monument-Monument","Pfeiler-Pfeiler","Upper Montane-Upper Montane")

pred_pd <- pred_pd %>% filter(originSite %in% site_ins,
                            !is.na(conf.low))

#split treatment column
pred_pd <- separate_wider_delim(pred_pd, cols = treatment, delim = "-", 
                               names = c("comparison1", "comparison2"))

#keep only treatments compared to within site
pred_pd <- pred_pd %>% filter(comparison1 %in% "within_site_transplant")

#multiply by neg 1
pred_pd$Contrast <- pred_pd$Contrast*-1
pred_pd$conf.low <- pred_pd$conf.low*-1
pred_pd$conf.high <- pred_pd$conf.high*-1

#redo site name values
pred_pd$originSite[pred_pd$originSite=="Monument-Monument"]<-"Monument"
pred_pd$originSite[pred_pd$originSite=="Pfeiler-Pfeiler"]<-"Pfeiler"
pred_pd$originSite[pred_pd$originSite=="Upper Montane-Upper Montane"]<-"Upper Montane"

#MPD----------
#bring in data
pred_mpd <- read.csv("ModelOutput/Prediction_mpd_nested.csv")

#remove weird site comparisons
site_ins <- c("Monument-Monument","Pfeiler-Pfeiler","Upper Montane-Upper Montane")

pred_mpd <- pred_mpd %>% filter(originSite %in% site_ins,
                              !is.na(conf.low))

#split treatment column
pred_mpd <- separate_wider_delim(pred_mpd, cols = treatment, delim = "-", 
                                names = c("comparison1", "comparison2"))

#keep only treatments compared to within site
pred_mpd <- pred_mpd %>% filter(comparison1 %in% "within_site_transplant")

#multiply by neg 1
pred_mpd$Contrast <- pred_mpd$Contrast*-1
pred_mpd$conf.low <- pred_mpd$conf.low*-1
pred_mpd$conf.high <- pred_mpd$conf.high*-1

#redo site name values
pred_mpd$originSite[pred_mpd$originSite=="Monument-Monument"]<-"Monument"
pred_mpd$originSite[pred_mpd$originSite=="Pfeiler-Pfeiler"]<-"Pfeiler"
pred_mpd$originSite[pred_mpd$originSite=="Upper Montane-Upper Montane"]<-"Upper Montane"

#MNTD---------
#bring in data
pred_mntd <- read.csv("ModelOutput/Prediction_mntd_nested.csv")

#remove weird site comparisons
site_ins <- c("Monument-Monument","Pfeiler-Pfeiler","Upper Montane-Upper Montane")

pred_mntd <- pred_mntd %>% filter(originSite %in% site_ins,
                              !is.na(conf.low))

#split treatment column
pred_mntd <- separate_wider_delim(pred_mntd, cols = treatment, delim = "-", 
                                names = c("comparison1", "comparison2"))

#keep only treatments compared to within site
pred_mntd <- pred_mntd %>% filter(comparison1 %in% "within_site_transplant")

#multiply by neg 1
pred_mntd$Contrast <- pred_mntd$Contrast*-1
pred_mntd$conf.low <- pred_mntd$conf.low*-1
pred_mntd$conf.high <- pred_mntd$conf.high*-1

#redo site name values
pred_mntd$originSite[pred_mntd$originSite=="Monument-Monument"]<-"Monument"
pred_mntd$originSite[pred_mntd$originSite=="Pfeiler-Pfeiler"]<-"Pfeiler"
pred_mntd$originSite[pred_mntd$originSite=="Upper Montane-Upper Montane"]<-"Upper Montane"

