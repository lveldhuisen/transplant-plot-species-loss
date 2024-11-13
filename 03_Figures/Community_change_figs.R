library(tidyverse)
library(ggeffects)
library(sjPlot)
library(dplyr)
library(patchwork)

#plots for changing shannon and phylogentic diversity 

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
  geom_point(alpha = .5, size = 1) +
  geom_line(aes(y=fit,group = turfID)) +
  labs(x = 'Time', y = 'Shannon diversity', color = 'Treatment') +
  theme_bw() +
  scale_color_viridis_d()
 # facet_wrap(.~originSite)

##significance of fixed effects#####
#bring in data
h_model <- readRDS("ModelOutput/Shannon_LMM.RDS")

plot_model(h_model,
           show.values=TRUE, show.p=TRUE,
           title="Effect of year and treatment on Shannon diversity")

##model output for predicted shannon#####
pred2 <- ggpredict(h_model, terms = c("treatment")) %>% 
  filter(x !="netted_untouched",
         x !="untouched")
pred2$x <- factor(pred2$x, 
                  levels = c("cooled_two_steps",
                             "cooled_one_step",
                             "within_site_transplant",
                             "warmed_one_step",
                             "warmed_two_steps"))

#figure
shannon_fig <- ggplot(pred2)+
  geom_pointrange(mapping = aes(x = x, y= predicted, ymin = conf.low, ymax = conf.high))+
  geom_hline(yintercept = 2.07, linetype = "dashed")+
  theme_bw()+
  xlab("Treatment") +
  ylab("Shannon diversity")+
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))

plot(shannon_fig)

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

pred3 <- ggpredict(model3, terms = c("treatment")) %>% 
  filter(x !="netted_untouched",
         x !="untouched")
pred3$x <- factor(pred3$x, 
                  levels = c("cooled_two_steps",
                             "cooled_one_step",
                             "within_site_transplant",
                             "warmed_one_step",
                             "warmed_two_steps"))

#try figure
pd_fig <- ggplot(pred3)+
  geom_pointrange(mapping = aes(x = x, y= predicted, ymin = conf.low, ymax = conf.high))+
  geom_hline(yintercept = -0.17, linetype = "dashed")+
  theme_bw()+
  xlab("Treatment") +
  ylab("PD")+
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))

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

pred4 <- ggpredict(model4, terms = c("treatment"))%>% 
  filter(x !="netted_untouched",
         x !="untouched")
pred4$x <- factor(pred4$x, 
                  levels = c("cooled_two_steps",
                             "cooled_one_step",
                             "within_site_transplant",
                             "warmed_one_step",
                             "warmed_two_steps"))

#figure for fixed effects
plot_model(model4,
           show.values=TRUE, show.p=TRUE,
           title="Effect of year and treatment on MPD")

##predicted MPD fig####
mpd_fig <- ggplot(pred4)+
  geom_pointrange(mapping = aes(x = x, y= predicted, ymin = conf.low, ymax = conf.high))+
  geom_hline(yintercept = -0.11, linetype = "dashed")+
  theme_bw()+
  xlab("Treatment") +
  ylab("MPD")+
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))

#MNTD-----------------------------
#bring in model results
model5 <- readRDS("ModelOutput/MNTD_LMM.RDS")

pred5 <- ggpredict(model5, terms = c("treatment"))%>% 
  filter(x !="netted_untouched",
         x !="untouched")

pred5$x <- factor(pred5$x, 
                  levels = c("cooled_two_steps",
                             "cooled_one_step",
                             "within_site_transplant",
                             "warmed_one_step",
                             "warmed_two_steps"))

##significance of fixed effects#######
plot_model(model5,
           show.values=TRUE, show.p=TRUE,type = "est",
           title="Effect of year and treatment on MNTD")

mntd_fig <- ggplot(pred5)+
  geom_pointrange(mapping = aes(x = x, y= predicted, ymin = conf.low, ymax = conf.high))+
  geom_hline(yintercept = -0.388, linetype = "dashed")+
  theme_bw()+
  xlab("Treatment") +
  ylab("MNTD")+
  scale_x_discrete(labels = c("-2", "-1", "0", "+1","+2"))

#combine figs-------------------
all_fig <- (shannon_fig + pd_fig) / (mpd_fig + mntd_fig)+
  plot_annotation(tag_levels = 'A')+
  plot_layout(axis_titles = "collect")
plot(all_fig)
