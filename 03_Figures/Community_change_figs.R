library(tidyverse)
library(ggeffects)
library(sjPlot)
library(dplyr)
library(patchwork)

#plots for changing shannon and phylogenetic diversity 

#Shannon diversity------------------------

##change over time figure, very chaotic#####
#bring in data
shannon_df <- read.csv("Data/h_dat.csv")

#use only within site transplant for control
control.outs <- c("netted_untouched","untouched")
shannon_df <- shannon_df %>% filter(!is.na(treatment),
                                          !treatment %in% control.outs)

#make plot
ggplot(shannon_df, aes(x = year, y = shannon_plots, color = treatment)) +
  geom_point(alpha = .5, size = 1, lty=originSite) +
  geom_line(aes(y=fit,group = turfID)) +
  labs(x = 'Time', y = 'Shannon diversity', color = 'Treatment') +
  theme_bw() +
  scale_color_viridis_d()
 #facet_wrap(.~originSite)

##significance of fixed effects#####
#bring in data
h_model <- readRDS("ModelOutput/Shannon_LMM.RDS")

plot_model(h_model,
           show.values=TRUE, show.p=TRUE,
           title="Effect of year and treatment on Shannon diversity")

##model output for predicted shannon#####
pred2 <- ggpredict(h_model, terms = c("treatment","originSite")) %>% 
  filter(x !="netted_untouched",
         x !="untouched")

#reorder groups
pred2$group <- factor(pred2$group,
                      levels  = c("Upper Montane",
                                  "Pfeiler",
                                  "Monument"))
pred2$x <- factor(pred2$x, 
                  levels = c("cooled_two_steps",
                             "cooled_one_step",
                             "within_site_transplant",
                             "warmed_one_step",
                             "warmed_two_steps"))

#make extra dataset to set different baseline horizontal lines in faceted fig
dummy_shannon <- data.frame(group = c("Upper Montane","Pfeiler","Monument"))
dummy_shannon$H <- c(2.37, 2.33, 2.07)
dummy_shannon$group <- factor(dummy_shannon$group,
                              levels  = c("Upper Montane",
                                          "Pfeiler",
                                          "Monument"))

#figure faceted by origin site
shannon_fig <- ggplot(pred2)+
  geom_pointrange(mapping = aes(x = x, y= predicted, ymin = conf.low, ymax = conf.high))+
  geom_hline(data= dummy_shannon, aes(yintercept=H), linetype = "dashed")+
  theme_bw()+
  xlab("Treatment") +
  ylab("Shannon diversity")+
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))+
  facet_wrap(.~group)

plot(shannon_fig)

#figure colored by origin site
ggplot(pred2)+
  geom_pointrange(mapping = aes(x = x, y= predicted, ymin = conf.low, ymax = conf.high, color= group))+
  geom_hline(yintercept=2.07, linetype = "dashed", color = "#FDE72FFF")+
  geom_hline(yintercept=2.33, linetype = "dashed", color = "#22A884FF")+
  geom_hline(yintercept=2.37, linetype = "dashed", color = "#414487FF")+
  theme_bw()+
  xlab("Treatment") +
  ylab("Shannon diversity")+
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))+
  scale_color_manual(values=c("#414487FF", "#22A884FF", "#FDE72FFF"))

#Richness--------------------

#bring in data
model_r <- readRDS("ModelOutput/Richness_LMM.RDS")

#make extra dataset to set different baseline horizontal lines in faceted fig
dummy_r <- data.frame(group = c("Upper Montane","Pfeiler","Monument"))
dummy_r$richness <- c(17.17, 15.65, 12.67)
dummy_r$group <- factor(dummy_r$group,
                              levels  = c("Upper Montane",
                                          "Pfeiler",
                                          "Monument"))

##significance of fixed effects#####
plot_model(model_r,
           show.values=TRUE, show.p=TRUE,
           title="Effect of year and treatment on species richness")

##model output for predicted shannon#####
pred_r <- ggpredict(model_r, terms = c("treatment","originSite")) %>% 
  filter(x !="netted_untouched",
         x !="untouched")
pred_r$x <- factor(pred_r$x, 
                  levels = c("cooled_two_steps",
                             "cooled_one_step",
                             "within_site_transplant",
                             "warmed_one_step",
                             "warmed_two_steps"))
#reorder groups
pred_r$group <- factor(pred_r$group,
                      levels  = c("Upper Montane",
                                  "Pfeiler",
                                  "Monument"))

#figure
richness_fig <- ggplot(pred_r)+
  geom_pointrange(mapping = aes(x = x, y= predicted, ymin = conf.low, ymax = conf.high))+
  geom_hline(data= dummy_r, aes(yintercept=richness), linetype = "dashed")+
  theme_bw()+
  xlab("Treatment") +
  ylab("Species richness")+
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))+
  facet_wrap(.~group)

plot(richness_fig)

#PD---------------
##change over time######
#bring in data
pd_df <- read.csv("Data/pd_dat.csv")

#remove 2017 data and additional controls
control.outs <- c("netted_untouched","untouched")
pd_df <- pd_df %>% filter(!year %in% 2017,
                          !treatment %in% control.outs)


#make plot
ggplot(pd_df, aes(x = year, y = pd.obs.z, color = treatment)) +
  geom_point(alpha = .5, size = 1) +
  geom_line(aes(group = turfID)) +
  labs(x = 'Time', y = 'PD', title = 'Change in PD over time', color = 'Treatment') +
  theme_bw() +
  scale_color_viridis_d()

##fixed effects significance####
plot_model(model3,show.values=TRUE, show.p=TRUE,
           title="Effect of year and treatment on PD")

##model output for predicted PD#####
#bring in model results
model3 <- readRDS("ModelOutput/PD_LMM.RDS")

pred3 <- ggpredict(model3, terms = c("treatment","originSite")) %>% 
  filter(x !="netted_untouched",
         x !="untouched")

pred3$x <- factor(pred3$x, 
                  levels = c("cooled_two_steps",
                             "cooled_one_step",
                             "within_site_transplant",
                             "warmed_one_step",
                             "warmed_two_steps"))

#reorder groups
pred3$group <- factor(pred3$group,
                      levels  = c("Upper Montane",
                                  "Pfeiler",
                                  "Monument"))

#make extra dataset to set different baseline horizontal lines in faceted fig
dummy_pd <- data.frame(group = c("Upper Montane","Pfeiler","Monument"))
dummy_pd$pd <- c(-0.75, -1.12, -0.17)
dummy_pd$group <- factor(dummy_pd$group,
                        levels  = c("Upper Montane",
                                    "Pfeiler",
                                    "Monument"))

#try figure
pd_fig <- ggplot(pred3)+
  geom_pointrange(mapping = aes(x = x, y= predicted, ymin = conf.low, ymax = conf.high))+
  geom_hline(data= dummy_pd, aes(yintercept=pd), linetype = "dashed")+
  theme_bw()+
  xlab("Treatment") +
  ylab("PD")+
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))+
  facet_wrap(.~group)

plot(pd_fig)

#MPD--------------------
#bring in data
mpd_dat <- read.csv("Data/MPD_byPlot18-23.csv")
mpd_dat <- mpd_dat %>% filter(!treatment %in% control.outs)

##change over time#######
#make plot
ggplot(mpd_dat, aes(x = year, y = mpd.obs.z, color = treatment)) +
  geom_point(alpha = .5, size = 1) +
  geom_line(aes(group = turfID)) +
  labs(x = 'Time', y = 'PD', title = 'Change in MPD over time', color = 'Treatment') +
  theme_bw() +
  scale_color_viridis_d()

##fixed effect significance
#bring in model results
model4 <- readRDS("ModelOutput/MPD_LMM.RDS")

pred4 <- ggpredict(model4, terms = c("treatment", "originSite"))%>% 
  filter(x !="netted_untouched",
         x !="untouched")
pred4$x <- factor(pred4$x, 
                  levels = c("cooled_two_steps",
                             "cooled_one_step",
                             "within_site_transplant",
                             "warmed_one_step",
                             "warmed_two_steps"))

#reorder groups
pred4$group <- factor(pred4$group,
                      levels  = c("Upper Montane",
                                  "Pfeiler",
                                  "Monument"))

#figure for fixed effects
plot_model(model4,
           show.values=TRUE, show.p=TRUE,
           title="Effect of year and treatment on MPD")

#make extra dataset to set different baseline horizontal lines in faceted fig
dummy_mpd <- data.frame(group = c("Upper Montane","Pfeiler","Monument"))
dummy_mpd$mpd <- c(-0.4,-0.6,-0.11)
dummy_mpd$group <- factor(dummy_pmd$group,
                         levels  = c("Upper Montane",
                                     "Pfeiler",
                                     "Monument"))

##predicted MPD fig####
mpd_fig <- ggplot(pred4)+
  geom_pointrange(mapping = aes(x = x, y= predicted, ymin = conf.low, ymax = conf.high))+
  geom_hline(data= dummy_mpd, aes(yintercept=mpd), linetype = "dashed")+
  theme_bw()+
  xlab("Treatment") +
  ylab("MPD")+
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))+
  facet_wrap(.~group)

plot(mpd_fig)

#MNTD-----------------------------
#bring in model results
model5 <- readRDS("ModelOutput/MNTD_LMM.RDS")

pred5 <- ggpredict(model5, terms = c("treatment", "originSite"))%>% 
  filter(x !="netted_untouched",
         x !="untouched")

pred5$x <- factor(pred5$x, 
                  levels = c("cooled_two_steps",
                             "cooled_one_step",
                             "within_site_transplant",
                             "warmed_one_step",
                             "warmed_two_steps"))

#reorder groups
pred5$group <- factor(pred5$group,
                      levels  = c("Upper Montane",
                                  "Pfeiler",
                                  "Monument"))

#make extra dataset to set different baseline horizontal lines in faceted fig
dummy_mntd <- data.frame(group = c("Upper Montane","Pfeiler","Monument"))
dummy_mntd$mntd <- c(-0.69,-1.15,-0.388)
dummy_mntd$group <- factor(dummy_pd$group,
                         levels  = c("Upper Montane",
                                     "Pfeiler",
                                     "Monument"))

##significance of fixed effects#######
plot_model(model5,
           show.values=TRUE, show.p=TRUE,type = "est",
           title="Effect of year and treatment on MNTD")

#predictions in figure 
mntd_fig <- ggplot(pred5)+
  geom_pointrange(mapping = aes(x = x, y= predicted, ymin = conf.low, ymax = conf.high))+
  geom_hline(data= dummy_mntd, aes(yintercept=mntd), linetype = "dashed")+
  theme_bw()+
  xlab("Treatment") +
  ylab("MNTD")+
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))+
  facet_wrap(.~group)

plot(mntd_fig)

#combine figs-------------------
all_fig <- (shannon_fig + pd_fig) / (mpd_fig + mntd_fig)+
  plot_annotation(tag_levels = 'A')+
  plot_layout(axis_titles = "collect")
plot(all_fig)
