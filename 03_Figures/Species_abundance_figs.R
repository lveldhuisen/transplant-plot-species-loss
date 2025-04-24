library(dplyr)
library(tidyverse)
library(viridis)
library(ggpubr)
library(patchwork)

#bring in data
slopes_df <- read.csv("Data/Species_change/Cover_slopes_all.csv")

#replace site names with elevations

slopes_df$originSite[slopes_df$originSite == 'Upper Montane'] <- 'Low elevation (2900 m)'
slopes_df$originSite[slopes_df$originSite == 'Pfeiler'] <- 'Middle elevation (3200 m)'
slopes_df$originSite[slopes_df$originSite == 'Monument'] <- 'High elevation (3300 m)'

#figures to display raw slope values----------

#reorder treatments and origin site
slopes_df$treatment <- factor(slopes_df$treatment, 
                         levels = c("cooled_two",
                                    "cooled_one",
                                    "within_site_transplant",
                                    "warmed_one",
                                    "warmed_two"))

slopes_df$originSite <- factor(slopes_df$originSite, 
                              levels = c("Low elevation (2900 m)",
                                         "Middle elevation (3200 m)",
                                         "High elevation (3300 m)"))

##heatmap-----
heat_labels <- c(
  expression(C[2]),
  expression(C[1]),
  "W/in",
  expression(W[1]), 
  expression(W[2]))

heatmap <- ggplot(slopes_df, aes(treatment, species, fill= slope)) + 
  geom_tile()+
  scale_fill_viridis(discrete = FALSE)+
  scale_x_discrete(labels = heat_labels)+
  theme_bw(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"))+
  facet_wrap(.~originSite)

plot(heatmap)
ggsave("Figures/Heatmap_supplement.png", dpi = 600, width = 14.5, height = 14.5)

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

nb_fig <- ggplot(slopes_df, aes(x=originally_at_destination., y= slope, colour = treatment))+
  geom_boxplot()+
  facet_wrap(.~originSite)+
  theme_bw(base_size = 20)+
  xlab("Observed at destination site pre-transplant?")+
  labs(colour = "Treatment")+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#35B779FF", "#AADC32FF","#FDE725FF"),
                     labels = c("Cooled two steps", "Cooled one step", "Within site transplant",
                                "Warmed one step", "Warmed two steps"))

plot(nb_fig)
ggsave("Figures/Fig5.png", dpi = 600, width = 14.5, height = 6)

#correlation between 2017 abundance and slope------
ab2017_df <- read.csv("Data/Species_change/2017abundance_slopes.csv")
ab2017_df$slope <- as.numeric(ab2017_df$slope)
ab2017_df$count.y <- as.numeric(ab2017_df$count.y)

#replace site names with elevations

ab2017_df$originSite[ab2017_df$originSite == 'Upper Montane'] <- 'Low elevation (2900 m)'
ab2017_df$originSite[ab2017_df$originSite == 'Pfeiler'] <- 'Middle elevation (3200 m)'
ab2017_df$originSite[ab2017_df$originSite == 'Monument'] <- 'High elevation (3300 m)'

#reorder treatments
ab2017_df$originSite <- factor(ab2017_df$originSite, 
                                levels = c("Low elevation (2900 m)",
                                           "Middle elevation (3200 m)",
                                           "High elevation (3300 m)"))

ab2017_df$treatment <- factor(ab2017_df$treatment, 
                               levels = c("cooled_two",
                                          "cooled_one",
                                          "within_site_transplant",
                                          "warmed_one",
                                          "warmed_two"))

#add 1 to all abundance averages for log
ab2017_df$count.y <- ab2017_df$count.y + 1

#figure                     
abundance17_fig <-  ggplot(ab2017_df, aes(x = log(count.y), y = slope, color = treatment))+
  geom_jitter(height =0, width = 0.1)+
  theme_bw(base_size = 20)+
  facet_wrap(.~originSite)+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#35B779FF", "#AADC32FF","#FDE725FF"), 
                     labels = c("Cooled two steps", "Cooled one step", "Within site transplant",
                     "Warmed one step", "Warmed two steps"))+
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = c(9.5,12,14.5,17,12), size = 4) +
  stat_regline_equation(label.y = c(8.5,11,13.5,16,11), size = 4)+
  xlab("Log of 2017 pre-transplant abundance")+
  labs(color = "Treatment")

plot(abundance17_fig)

#correlation between range size and slope--------
aoo_slopes <- read.csv("Data/Species_change/Cover_slopes_all.csv")

#plot
rs_fig <- ggplot(slopes_df, aes(x=log(AOO), y=slope, color = treatment))+
  geom_point()+
  theme_bw(base_size = 20)+
  labs(x= "Log of range size")+
  facet_wrap(.~originSite)+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#35B779FF", "#AADC32FF",
                              "#FDE725FF"), 
                     labels = c("Cooled two steps", "Cooled one step", "Within site transplant",
                                "Warmed one step", "Warmed two steps"))+
  geom_smooth(method = "lm", se = FALSE)+
  stat_cor(label.y = c(9.5,12,14.5,17,12), size = 4) +
  stat_regline_equation(label.y = c(8.5,11,13.5,16,11), size = 4)+
  labs(color = "Treatment")

plot(rs_fig)

#combine regression figures
regression_fig <- abundance17_fig / rs_fig + 
  plot_annotation(tag_levels = c('A'), tag_suffix = ')')+
  plot_layout(guides = 'collect')

plot(regression_fig)
ggsave("Figures/Fig6.png", dpi = 600, height = 10.5, width = 14.5)
