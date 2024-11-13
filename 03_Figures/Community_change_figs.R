library(tidyverse)
library(ggeffects)
library(sjPlot)
library(dplyr)

#plots for changing shannon and phylogentic diversity over time

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
  scale_color_viridis_d()+
  facet_wrap(.~originSite)

##plot significance of fixed effects#####
#bring in data
h_model <- readRDS("ModelOutput/Shannon_LMM.RDS")

plot_model(h_model,
           show.values=TRUE, show.p=TRUE,
           title="Effect of year and treatment on Shannon diversity")

##plot model output for predicted shannon#####
pred2 <- ggpredict(h_model, terms = c("treatment")) %>% 
  filter(x !="netted_untouched",
         x !="untouched")
pred2$x <- factor(pred2$x, 
                  levels = c("cooled_two_steps",
                             "cooled_one_step",
                             "within_site_transplant",
                             "warmed_one_step",
                             "warmed_two_steps"))

#try figure
ggplot(pred2)+
  geom_pointrange(mapping = aes(x = x, y= predicted, ymin = conf.low, ymax = conf.high))+
  geom_hline(yintercept = 2.07, linetype = "dashed")+
  theme_bw()+
  xlab("Treatment") +
  ylab("predicted Shannon diversity")

#PD---------------
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

#MPD--------------------
#bring in data
mpd_dat <- read.csv("Data/MPD_byPlot18-23.csv")
mpd_dat <- mpd_dat %>% filter(!treatment %in% control.outs)

#make plot
ggplot(mpd_dat, aes(x = year, y = mpd.obs.z, color = treatment)) +
  geom_point(alpha = .5, size = 1) +
  geom_line(aes(group = turfID)) +
  labs(x = 'Time', y = 'PD', title = 'Change in MPD over time', color = 'Treatment') +
  theme_bw() +
  scale_color_viridis_d()


