library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(ggpmisc)
library(plyr)
library(rstatix)

#bring in data
aoo_slopes <- read.csv("Data/Species_change/Abundance_slopes_all.csv")

#add column to have origin site and tx in same column 
aoo_slopes$group <- paste(aoo_slopes$originSite,"_",aoo_slopes$treatment)

#niche breadth (y/n at destination site pre-transplant)--------------
results <- aoo_slopes %>%
  group_by(group) %>%
  group_map(~ t.test(slope ~ originally_at_destination., .x))

aoo_slopes %>%
  group_by(group) %>%
  summarise(p.value = t.test(slope ~ group)$p.value) %>%
  ungroup()

#figure
ggplot(aoo_slopes, aes(x=slope))+
  geom_histogram()+
  facet_wrap(.~group)

ggplot(aoo_slopes, aes(x=originally_at_destination., y=slope))+
  geom_boxplot()+
  theme_bw()+
  labs(y = expression(Delta ~ "abundance"), x= "log(range size)")+
  facet_wrap(.~group)

#relationship between abundance change and range size------------

#all sites/tx combined
aoo_model <- lm(slope ~ AOO, data=aoo_slopes)
summary(aoo_model)

#separated out by tx/origin
rs_model <- dlply(aoo_slopes,"group",function(aoo_slopes) lm(slope ~ AOO, 
                                                                   data = aoo_slopes))
rs_values <- ldply(rs_model,coef)


