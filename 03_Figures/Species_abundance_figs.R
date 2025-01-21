library(dplyr)
library(tidyverse)
library(viridis)
library(ggpubr)

#bring in data
slopes_df <- read.csv("Data/Species_change/Cover_slopes_all.csv")

#figures to display raw slope values----------

#reorder treatments and origin site
slopes_df$treatment <- factor(slopes_df$treatment, 
                         levels = c("cooled_two",
                                    "cooled_one",
                                    "within_site_transplant",
                                    "warmed_one",
                                    "warmed_two"))

slopes_df$originSite <- factor(slopes_df$originSite, 
                              levels = c("Upper Montane",
                                         "Pfeiler",
                                         "Monument"))

##heatmap-----
ggplot(slopes_df, aes(treatment, species, fill= slope)) + 
  geom_tile()+
  scale_fill_viridis(discrete = FALSE)+
  scale_x_discrete(labels = c("C2", "C1", "Within site transplant", "W1","W2"))+
  theme_bw()+
  theme(axis.text.y = element_text(face = "italic"))+
  facet_wrap(.~originSite)

##histograms------
ggplot(slopes_df, aes(x=slope)) + 
  geom_histogram()+
  theme_bw()+
  facet_wrap(originSite ~ treatment)

##violin plots-----
ggplot(slopes_df, aes(x=treatment, y=slope))+
  geom_violin()+
  scale_x_discrete(labels = c("C2", "C1", "Within site transplant", "W1","W2"))+
  theme_bw()+
  facet_wrap(.~originSite)

#niche breadth: slopes depending if species in destination site pre-transplant----

ggplot(slopes_df, aes(x=originally_at_destination., y= slope, colour = treatment))+
  geom_boxplot()+
  facet_wrap(.~originSite)+
  theme_bw()+
  xlab("Existed at destination site pre-transplant?")+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#35B779FF", "#AADC32FF","#FDE725FF"))
  stat_pvalue_manual(stat.test, label = "p.adj.signif")

#correlation between 2017 abundance and slope------
aoo_slopes <- read.csv("Data/Species_change/complete_species.csv")

#reorder treatments
aoo_slopes$originSite <- factor(aoo_slopes$originSite, 
                                levels = c("Upper Montane",
                                           "Pfeiler",
                                           "Monument"))

aoo_slopes$treatment <- factor(aoo_slopes$treatment, 
                               levels = c("cooled_two",
                                          "cooled_one",
                                          "within_site_transplant",
                                          "warmed_one",
                                          "warmed_two"))

#figure                     
ggplot(aoo_slopes, aes(x = log(occurrenceCount.y), y = slope, color = treatment))+
  geom_point()+
  theme_bw()+
  facet_wrap(.~originSite)+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#35B779FF", "#AADC32FF","#FDE725FF"))+
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = c(9.5,12,14.5,17,12), size = 4) +
  stat_regline_equation(label.y = c(8.5,11,13.5,16,11), size = 4)+
  xlab("2017 pre-transplant abundance")


#correlation between range size and slope--------
aoo_slopes <- read.csv("Data/Species_change/Cover_slopes_all.csv")

#plot
ggplot(slopes_df, aes(x=log(AOO), y=slope, color = treatment))+
  geom_point()+
  theme_bw()+
  labs(x= "log(range size)")+
  facet_wrap(.~originSite)+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#35B779FF", "#AADC32FF",
                              "#FDE725FF"))+
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = c(9.5,12,14.5,17,12), size = 4) +
  stat_regline_equation(label.y = c(8.5,11,13.5,16,11), size = 4)

  
