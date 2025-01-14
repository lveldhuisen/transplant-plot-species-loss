library(dplyr)
library(tidyverse)
library(viridis)
library(ggpubr)

#bring in data
slopes_df <- read.csv("Data/Species_change/Abundance_slopes_all.csv")

#figures to display raw slope values----------

#reorder treatments
slopes_df$treatment <- factor(slopes_df$treatment, 
                         levels = c("cooled_two",
                                    "cooled_one",
                                    "within_site_transplant",
                                    "warmed_one",
                                    "warmed_two"))

##heatmap-----
ggplot(slopes_df, aes(treatment, species, fill= slope)) + 
  geom_tile()+
  scale_fill_viridis(discrete = FALSE)+
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))+
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
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))+
  theme_bw()+
  facet_wrap(.~originSite)

#correlation between range size and slope--------
aoo_slopes <- read.csv("Data/Species_change/Abundance_slopes_all.csv")

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
#plot
ggplot(aoo_slopes, aes(x=log(AOO), y=slope, color = treatment))+
  geom_point()+
  theme_bw()+
  labs(y = expression(Delta ~ "abundance (by species)"), x= "log(range size)")+
  facet_wrap(.~originSite)+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#35B779FF", "#8FD744FF","#FDE725FF"))+
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = c(11,13,17,19,21)) +
  stat_regline_equation(label.y = c(10,12,16,18,20))

  
