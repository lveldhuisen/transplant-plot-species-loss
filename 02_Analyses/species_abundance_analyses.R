library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)
library(ggpmisc)
library(plyr)
library(rstatix)

#bring in data
aoo_slopes <- read.csv("Data/Species_change/Cover_slopes_all_2.csv")

#add column to have origin site and tx in same column 
aoo_slopes$group <- paste(aoo_slopes$originSite,"_",aoo_slopes$treatment)

#niche breadth (y/n at destination site pre-transplant)--------------

stat.test <- aoo_slopes %>%
  group_by(group) %>%
  t_test(slope ~ originally_at_destination.) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  add_y_position()

# Check sample sizes using table
table(aoo_slopes$group, aoo_slopes$originally_at_destination.)

view(stat.test)

#figure
ggplot(aoo_slopes, aes(x=slope))+
  geom_histogram()+
  facet_wrap(.~group)

## niche breadth test with ICV values for revision #######


# Check sample sizes using table
table(icv_df_naomit$group, icv_df_naomit$originally_at_destination.)

# only one data point in pfeiler w/in site transplant no group - filter out 
stat_test_icv <- icv_df_naomit %>%
  filter(group != "Mid elevation (3200 m) _ within_site_transplant") %>%
  group_by(group) %>%
  t_test(icv ~ originally_at_destination.) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance() %>%
  add_y_position()

view(stat_test_icv)

#does original abundance predict abundance change?------------------------
ab2017_df <- read.csv("Data/Species_change/2017abundance_slopes.csv")

model2 <- lm(ab2017_df$slope ~ ab2017_df$count.y, data = ab2017_df)
summary(model2)

ggplot(ab2017_df, aes(x = log(count.y), y = slope, color = treatment))+
  geom_point()+
  theme_bw()+
  facet_wrap(.~originSite)

#relationship between abundance change and range size------------

#all sites/tx combined
aoo_model <- lm(slope ~ AOO, data=aoo_slopes)
summary(aoo_model)

#separated out by tx/origin
rs_model <- dlply(aoo_slopes,"group",function(aoo_slopes) lm(slope ~ AOO, 
                                                                   data = aoo_slopes))
rs_values <- ldply(rs_model,coef)

mean(aoo_slopes$slope)

