library(dplyr)
library(tidyverse)
library(viridis)
library(ggpubr)
library(patchwork)

#bring in data
icv_df <- read.csv("Data/Species_change/all_changes_forrevision.csv")

#replace site names with elevations

icv_df$originSite[icv_df$originSite == 'Upper Montane'] <- 'Low elevation (2900 m)'
icv_df$originSite[icv_df$originSite == 'Pfeiler'] <- 'Mid elevation (3200 m)'
icv_df$originSite[icv_df$originSite == 'Monument'] <- 'High elevation (3300 m)'

#figure to display raw icv values----------

#reorder treatments and origin site
icv_df$treatment <- factor(icv_df$treatment, 
                              levels = c("cooled_two",
                                         "cooled_one",
                                         "within_site_transplant",
                                         "warmed_one",
                                         "warmed_two"))

icv_df$originSite <- factor(icv_df$originSite, 
                               levels = c("Low elevation (2900 m)",
                                          "Mid elevation (3200 m)",
                                          "High elevation (3300 m)"))

icv_df_naomit <- na.omit(icv_df)


#add column to have origin site and tx in same column 
icv_df_naomit$group <- paste(icv_df_naomit$originSite,"_",icv_df_naomit$treatment)

##heatmap-----
heat_labels <- c(
  expression(C[2]),
  expression(C[1]),
  "W/in",
  expression(W[1]), 
  expression(W[2]))

heatmap_icv <- ggplot(icv_df, aes(treatment, species, fill= icv)) + 
  geom_tile()+
  scale_fill_viridis(discrete = FALSE)+
  scale_x_discrete(labels = heat_labels)+
  theme_bw(base_size = 20)+
  theme(axis.text.y = element_text(face = "italic"))+
  facet_wrap(.~originSite)+
  xlab("Treatment") + 
  ylab("species")

plot(heatmap_icv)
ggsave("Figures/Heatmap_supplement_withICV.png", dpi = 600, width = 14.5, height = 14.5)


#niche breadth: slopes depending if species in destination site pre-transplant----

nb_fig_rev <- ggplot(icv_df_naomit, aes(x=originally_at_destination., y= icv, colour = treatment))+
  geom_boxplot()+
  facet_wrap(.~originSite)+
  theme_bw(base_size = 20)+
  labs(y = expression(Delta ~ "cover"), 
       x = "Observed at destination site pre-transplant?", 
       colour = "Treatment")+
  annotate("text", label = "All~italic(p)~'>'~0.05", size = 5.5, y = 9.5, x = 1.3, parse = TRUE) +
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#35B779FF", "#AADC32FF","#FDE725FF"),
                     labels = c("Cooled two steps", "Cooled one step", "Local transplant",
                                "Warmed one step", "Warmed two steps"))

plot(nb_fig_rev)
ggsave("Figures/Fig5_ICV_revised.png", dpi = 600, width = 14.5, height = 6)

#correlation between 2017 abundance and slope------
#ab2017_df <- read.csv("Data/Species_change/2017abundance_slopes.csv")


#replace site names with elevations

icv_2017ab$originSite[icv_2017ab$originSite == 'Upper Montane'] <- 'Low elevation (2900 m)'
icv_2017ab$originSite[icv_2017ab$originSite == 'Pfeiler'] <- 'Mid elevation (3200 m)'
icv_2017ab$originSite[icv_2017ab$originSite == 'Monument'] <- 'High elevation (3300 m)'

#reorder treatments
icv_2017ab$originSite <- factor(icv_2017ab$originSite, 
                               levels = c("Low elevation (2900 m)",
                                          "Mid elevation (3200 m)",
                                          "High elevation (3300 m)"))

icv_2017ab$treatment <- factor(icv_2017ab$treatment, 
                              levels = c("cooled_two",
                                         "cooled_one",
                                         "within_site_transplant",
                                         "warmed_one",
                                         "warmed_two"))

#add 1 to all abundance averages for log
icv_2017ab$count.y <- icv_2017ab$count.y + 1

#figure                     
abundance17_fig_rev <-  ggplot(icv_2017ab, aes(x = log(count.y), y = icv, color = treatment))+
  geom_jitter(height =0, width = 0.1)+
  theme_bw(base_size = 20)+
  facet_wrap(.~originSite)+
  stat_cor(aes(label = paste(gsub("R", "r", after_stat(r.label)), after_stat(p.label), sep = "~`,`~")),
           label.y = c(11.5,10, 13, 14.5), size = 5.5) +
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#35B779FF", "#AADC32FF","#FDE725FF"), 
                     labels = c("Cooled two steps", "Cooled one step", "Local transplant",
                                "Warmed one step", "Warmed two steps"))+
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed")+
  labs(color = "Treatment", 
       x = "Log of 2017 pre-transplant abundance",
       y = expression(Delta ~ "cover"))

plot(abundance17_fig_rev)
ggsave("Figures/fig6a.pdf", height = 7, width = 15)

#correlation between range size and slope-------
test_icv <- icv_df %>% drop_na(AOO)

#plot
rs_fig_rev <- ggplot(test_icv, aes(x=log(AOO), y=icv, color = treatment))+
  geom_point()+
  theme_bw(base_size = 20)+
  labs(color = "Treatment",
    x = "Log of range size", 
    y = expression(Delta ~ "cover"))+
  facet_wrap(.~originSite)+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#35B779FF", "#AADC32FF","#FDE725FF"), 
                     labels = c("Cooled two steps", "Cooled one step", "Local transplant",
                                "Warmed one step", "Warmed two steps"))+
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed")+
  stat_cor(aes(label = paste(gsub("R", "r", after_stat(r.label)), after_stat(p.label), sep = "~`,`~")),
           label.y = c(11.5,10, 13, 14.5), size = 5.5)+
  theme(legend.position = 'none')

plot(rs_fig_rev)
ggsave("Figures/rangesize_noequations.png", dpi = 600, width = 16, height = 5)

#combine regression figures

regression_fig_rev <- abundance17_fig_rev / rs_fig_rev + 
  plot_annotation(tag_levels = c('A'), tag_suffix = ')')+
  plot_layout(guides = 'collect') 

plot(regression_fig_rev)
ggsave("Figures/Fig6_revised_with_icv.png", height = 11.5, width = 14.5, dpi = 600)
