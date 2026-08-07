library(tidyverse) #plots and data manipulation
library(ggeffects) #model outputs
library(sjPlot) #plots
library(dplyr) #data manipulation
library(patchwork) #combine plots

#site-level plots from the nested LMMs showing changes in community diversity 
#metrics relative to the within site transplant 

#Richness--------------------

#pred_R <- read.csv("ModelOutput/Prediction_richness_nested.csv")

#take pred_r object from Aug2026 LMM data prep script 

#subscripts in axis labels
tx_labels <- c(
  expression(C[2]),
  expression(C[1]),
  expression(W[1]), 
  expression(W[2]))

#figure 
richness_fig_site <- ggplot(pred_R)+
  geom_pointrange(mapping = aes(x = contrast, y= estimate, 
                                ymin = ymin,
                                ymax = ymax, 
                                color=originSite), 
                  position = position_dodge(width = 0.2),
                  size = 0.8,
                  linewidth = 1.3)+
  theme_classic(base_size = 26)+
  labs(y = expression(Delta ~ "species richness"), x = "Treatment")+
  scale_x_discrete(labels = tx_labels)+
  ylim(-6,6)+
  scale_color_manual(values=c("#8FD744FF","#287C8EFF","#440154FF"))+
  labs(color='Origin site')+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.8)+
  geom_text(aes(x = contrast, y = ymax + 0.5, label = sig, color = originSite),
              position = position_dodge(width = 0.2),
              size = 12, show.legend = FALSE)

plot(richness_fig_site)

#Shannon diversity------------------------
#take pred_S object from Aug2026 LMM data prep script 

#figure 
shannon_fig_site <- ggplot(pred_S)+
  geom_pointrange(mapping = aes(x = contrast, y= estimate, 
                                ymin = ymin,
                                ymax = ymax, 
                                color=originSite), 
                  position = position_dodge(width = 0.2), 
                  size = 0.8,
                  linewidth = 1.3)+
  theme_classic(base_size = 26)+
  labs(y = expression(Delta ~ "Shannon diversity"), x = "Treatment")+
  scale_x_discrete(labels = tx_labels)+
  scale_color_manual(values=c("#8FD744FF","#287C8EFF","#440154FF"))+
  labs(color='Origin site')+
  ylim(-0.5, 0.60) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.8)+
  geom_text(aes(x = contrast, y = ymax + 0.1, label = sig, color = originSite),
            position = position_dodge(width = 0.2),
            size = 12, show.legend = FALSE)


plot(shannon_fig_site)

#PD---------------


#figure 
pd_fig_site <- ggplot(pred_PD)+
  geom_pointrange(mapping = aes(x = contrast, y= estimate, 
                                ymin = ymin,
                                ymax = ymax, 
                                color=originSite), 
                  position = position_dodge(width = 0.2), 
                  size = 0.8,
                  linewidth = 1.3)+
  theme_classic(base_size = 26)+
  labs(y = expression(Delta ~ "PD"), x = "Treatment")+
  scale_x_discrete(labels = tx_labels)+
  scale_color_manual(values=c("#8FD744FF","#287C8EFF","#440154FF"))+
  labs(color='Origin site')+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.8)+
  geom_text(aes(x = contrast, y = ymax + 0.1, label = sig, color = originSite),
            position = position_dodge(width = 0.2),
            size = 12, show.legend = FALSE)


plot(pd_fig_site)


#MPD--------------------


#figure 
mpd_fig_site <- ggplot(pred_MPD)+
  geom_pointrange(mapping = aes(x = contrast, y= estimate, 
                                ymin = ymin,
                                ymax = ymax,  
                                color=originSite), 
                  position = position_dodge(width = 0.2), 
                  size = 0.8, 
                  linewidth = 1.3)+
  theme_classic(base_size = 26)+
  labs(y = expression(Delta ~ "MPD"), x = "Treatment")+
  scale_x_discrete(labels = tx_labels)+
  scale_color_manual(values=c("#8FD744FF","#287C8EFF","#440154FF"))+
  labs(color='Origin site')+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.8)+
  geom_text(aes(x = contrast, y = ymax + 0.1, label = sig, color = originSite),
            position = position_dodge(width = 0.2),
            size = 12, show.legend = FALSE)

plot(mpd_fig_site)

#MNTD-----------------------------

#figure 
mntd_fig_site <- ggplot(pred_MNTD)+
  geom_pointrange(mapping = aes(x = contrast, y= estimate, 
                                ymin = ymin,
                                ymax = ymax,  
                                color=originSite),
                  position = position_dodge(width = 0.2),
                  size = 0.8, 
                  linewidth = 1.3)+
  theme_classic(base_size = 26)+
  labs(y = expression(Delta ~ "MNTD"), x = "Treatment")+
  scale_x_discrete(labels = tx_labels)+
  scale_color_manual(values=c("#8FD744FF","#287C8EFF","#440154FF"))+
  labs(color='Origin site')+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.8)+
  geom_text(aes(x = contrast, y = ymax + 0.1, label = sig, color = originSite),
            position = position_dodge(width = 0.2),
            size = 12, show.legend = FALSE)

plot(mntd_fig_site)

#combine figs with Patchwork-------------------

#two panels with shannon and richness
sr_fig <- (richness_fig_site + shannon_fig_site) + 
  plot_annotation(tag_levels = 'A', tag_suffix = ')')+
  plot_layout(axis_titles = "collect", guides = "collect")

plot(sr_fig)
ggsave("Figures/Fig2_revAug26.jpeg", dpi = 600, width = 15.5, height = 6)

#combine all phylo metrics
phylo_fig <- (pd_fig_site + mpd_fig_site + mntd_fig_site)+
  plot_annotation(tag_levels = c('A'), tag_suffix = ')')+
  plot_layout(axis_titles = "collect", guides = "collect")

plot(phylo_fig)
ggsave("Figures/Fig3_revAug26.jpeg", dpi = 600, width = 15.5, height = 5)


