library(tidyverse) #plots and data manipulation
library(ggeffects) #model outputs
library(sjPlot) #plots
library(dplyr) #data manipulation
library(patchwork) #combine plots

#site-level plots from the nested LMMs showing changes in community diversity 
#metrics relative to the within site transplant 

#Richness--------------------
pred_R <- read.csv("ModelOutput/Prediction_richness_nested.csv")

#reorder groups
pred_R$originSite <- factor(pred_R$originSite,
                            levels  = c("Low elevation",
                                        "Middle elevation",
                                        "High elevation"))

pred_R$comparison2 <- factor(pred_R$comparison2, 
                             levels = c("cooled_two_steps",
                                        "cooled_one_step",
                                        "warmed_one_step",
                                        "warmed_two_steps"))
#subscripts in axis labels
tx_labels <- c(
  expression(C[2]),
  expression(C[1]),
  expression(W[1]), 
  expression(W[2]))

#figure 
richness_fig_site <- ggplot(pred_R)+
  geom_pointrange(mapping = aes(x = comparison2, y= Contrast, 
                                ymin = conf.high,
                                ymax = conf.low, 
                                color=originSite), 
                  position = position_dodge(width = 0.2),
                  size = 0.8,
                  linewidth = 1.3)+
  theme_classic(base_size = 22)+
  labs(y = expression(Delta ~ "species richness"), x = "Treatment")+
  scale_x_discrete(labels = c("C2", "C1","W1","W2"))+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#8FD744FF"))+
  labs(color='Origin site')+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.3)

plot(richness_fig_site)

#Shannon diversity------------------------

#reorder groups
pred_s <- read.csv("Data/Shannon_model_forfig.csv")

pred_s$originSite <- factor(pred_s$originSite,
                            levels  = c("Low elevation",
                                        "Middle elevation",
                                        "High elevation"))

pred_s$comparison2 <- factor(pred_s$comparison2, 
                             levels = c("cooled_two_steps",
                                        "cooled_one_step",
                                        "warmed_one_step",
                                        "warmed_two_steps"))

#figure 
shannon_fig_site <- ggplot(pred_s)+
  geom_pointrange(mapping = aes(x = comparison2, y= Contrast, 
                                ymin = conf.high,
                                ymax = conf.low, 
                                color=originSite), 
                  position = position_dodge(width = 0.2), 
                  size = 0.8,
                  linewidth = 1.3)+
  theme_classic(base_size = 22)+
  labs(y = expression(Delta ~ "Shannon diversity"), x = "Treatment")+
  scale_x_discrete(labels = c("C2", "C1","W1","W2"))+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#8FD744FF"))+
  labs(color='Origin site')+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.3)

plot(shannon_fig_site)

#PD---------------

#reorder groups
pred_pd$originSite <- factor(pred_pd$originSite,
                            levels  = c("Low elevation",
                                        "Middle elevation",
                                        "High elevation"))

pred_pd$comparison2 <- factor(pred_pd$comparison2, 
                             levels = c("cooled_two_steps",
                                        "cooled_one_step",
                                        "warmed_one_step",
                                        "warmed_two_steps"))

#figure 
pd_fig_site <- ggplot(pred_pd)+
  geom_pointrange(mapping = aes(x = comparison2, y= Contrast, 
                                ymin = conf.high,
                                ymax = conf.low, 
                                color=originSite), 
                  position = position_dodge(width = 0.2), 
                  size = 0.8,
                  linewidth = 1.3)+
  theme_classic(base_size = 22)+
  labs(y = expression(Delta ~ "PD"), x = "Treatment")+
  scale_x_discrete(labels = tx_labels)+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#8FD744FF"))+
  labs(color='Origin site')+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.3)

plot(pd_fig_site)


#MPD--------------------

#reorder groups
pred_mpd$originSite <- factor(pred_mpd$originSite,
                             levels  = c("Low elevation",
                                         "Middle elevation",
                                         "High elevation"))

pred_mpd$comparison2 <- factor(pred_mpd$comparison2, 
                              levels = c("cooled_two_steps",
                                         "cooled_one_step",
                                         "warmed_one_step",
                                         "warmed_two_steps"))

#figure 
mpd_fig_site <- ggplot(pred_mpd)+
  geom_pointrange(mapping = aes(x = comparison2, y= Contrast, 
                                ymin = conf.high,
                                ymax = conf.low, 
                                color=originSite), 
                  position = position_dodge(width = 0.2), 
                  size = 0.8, 
                  linewidth = 1.3)+
  theme_classic(base_size = 22)+
  labs(y = expression(Delta ~ "MPD"), x = "Treatment")+
  scale_x_discrete(labels = tx_labels)+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#8FD744FF"))+
  labs(color='Origin site')+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.3)

plot(mpd_fig_site)

#MNTD-----------------------------

#reorder groups
pred_mntd$originSite <- factor(pred_mntd$originSite,
                              levels  = c("Low elevation",
                                          "Middle elevation",
                                          "High elevation"))

pred_mntd$comparison2 <- factor(pred_mntd$comparison2, 
                               levels = c("cooled_two_steps",
                                          "cooled_one_step",
                                          "warmed_one_step",
                                          "warmed_two_steps"))

#figure 
mntd_fig_site <- ggplot(pred_mntd)+
  geom_pointrange(mapping = aes(x = comparison2, y= Contrast, 
                                ymin = conf.high,
                                ymax = conf.low, 
                                color=originSite), 
                  position = position_dodge(width = 0.2),
                  size = 0.8, 
                  linewidth = 1.3)+
  theme_classic(base_size = 22)+
  labs(y = expression(Delta ~ "MNTD"), x = "Treatment")+
  scale_x_discrete(labels = tx_labels)+
  scale_color_manual(values=c("#440154FF", "#287C8EFF", "#8FD744FF"))+
  labs(color='Origin site')+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 1.3)

plot(mntd_fig_site)

#combine figs with Patchwork-------------------

#two panels with shannon and richness
sr_fig <- (richness_fig_site + shannon_fig_site) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(axis_titles = "collect", guides = "collect")

plot(sr_fig)

#combine all phylo metrics
phylo_fig <- (pd_fig_site + mpd_fig_site + mntd_fig_site)+
  plot_annotation(tag_levels = c('A'), tag_suffix = ')')+
  plot_layout(axis_titles = "collect", guides = "collect")

plot(phylo_fig)
ggsave("Figures/Fig3.pdf", dpi = 600, width = 14.5, height = 5)


