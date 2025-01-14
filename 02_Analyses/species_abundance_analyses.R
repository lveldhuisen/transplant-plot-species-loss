library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(ggpmisc)
library(plyr)

#bring in data
aoo_slopes <- read.csv("Data/Species_change/Abundance_slopes_all.csv")

#add column to have origin site and tx in same column 
aoo_slopes$group <- paste(aoo_slopes$originSite,"_",aoo_slopes$treatment)

#relationship between abundance change and range size------------

#all sites/tx combined
aoo_model <- lm(slope ~ AOO, data=aoo_slopes)
summary(aoo_model)

#separated out by tx/origin
rs_model <- dlply(aoo_slopes,"group",function(aoo_slopes) lm(slope ~ AOO, 
                                                                   data = aoo_slopes))
rs_values <- ldply(rs_model,coef)

#plot
ggplot(aoo_slopes, aes(x=log(AOO), y=slope, color = treatment))+
  geom_point()+
  theme_bw()+
  facet_wrap(.~originSite)+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#35B779FF", "#8FD744FF","#FDE725FF"))

