library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(ggeffects)
library(sjPlot)
library(vegan)
library(stringr)
library(dplyr)
library(merTools)
library(lme4)
library(broom.mixed)
library(ggplot2)
library(dplyr)
library(emmeans)

# CONVERT LME OUTPUT TO EMMEANS FOR PLOTTING -------

## richness -------

# get comparisons 
emm_R <- emmeans(model_r, ~ treatment | originSite)

contrast_R <- contrast(emm_R, method = "trt.vs.ctrl", 
                       ref = "within_site_transplant")
pred_R <- as.data.frame(contrast_R)

pred_R <- pred_R %>%
  mutate(ymin = estimate - SE,
         ymax = estimate + SE)

# rename treatment comparisons
pred_R <- pred_R %>%
  mutate(contrast = recode(contrast,
                             "warmed_one_step - within_site_transplant" = "Warmed one",
                             "warmed_two_steps - within_site_transplant" = "Warmed two",
                             "cooled_one_step - within_site_transplant" = "Cooled one",
                             "cooled_two_steps - within_site_transplant" = "Cooled two"),
         contrast = factor(contrast,
                             levels = c("Cooled two", "Cooled one", "Warmed one", "Warmed two")))

#rename treatment origin sites
pred_R <- pred_R %>%
  mutate(originSite = recode(originSite,
                             "Upper Montane" = "Low elevation",
                             "Pfeiler" = "Mid elevation",
                             "Monument" = "High elevation"),
         originSite = factor(originSite,
                             levels = c("Low elevation", "Mid elevation", "High elevation")))

#reorder groups
pred_R$originSite <- factor(pred_R$originSite,
                            levels  = c("Low elevation",
                                        "Mid elevation",
                                        "High elevation"))

pred_R$contrast <- factor(pred_R$contrast,
                            levels  = c("Cooled two",
                                        "Cooled one",
                                        "Warmed one",
                                        "Warmed two"))
# add asterisks for significance 
pred_R <- pred_R %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  ))
## Shannon --------

emm_S <- emmeans(model2_n, ~ treatment | originSite)

contrast_S <- contrast(emm_S, method = "trt.vs.ctrl", 
                       ref = "within_site_transplant")
pred_S <- as.data.frame(contrast_S)

pred_S <- pred_S %>%
  mutate(ymin = estimate - SE,
         ymax = estimate + SE)

# rename treatment comparisons
pred_S <- pred_S %>%
  mutate(contrast = recode(contrast,
                           "warmed_one_step - within_site_transplant" = "Warmed one",
                           "warmed_two_steps - within_site_transplant" = "Warmed two",
                           "cooled_one_step - within_site_transplant" = "Cooled one",
                           "cooled_two_steps - within_site_transplant" = "Cooled two"),
         contrast = factor(contrast,
                           levels = c("Cooled two", "Cooled one", "Warmed one", "Warmed two")))

#rename treatment origin sites
pred_S <- pred_S %>%
  mutate(originSite = recode(originSite,
                             "Upper Montane" = "Low elevation",
                             "Pfeiler" = "Mid elevation",
                             "Monument" = "High elevation"),
         originSite = factor(originSite,
                             levels = c("Low elevation", "Mid elevation", "High elevation")))

#reorder groups
pred_S$originSite <- factor(pred_S$originSite,
                            levels  = c("Low elevation",
                                        "Mid elevation",
                                        "High elevation"))

pred_S$contrast <- factor(pred_S$contrast,
                          levels  = c("Cooled two",
                                      "Cooled one",
                                      "Warmed one",
                                      "Warmed two"))

pred_S <- pred_S %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  ))

## PD ---------

emm_PD <- emmeans(model3_n, ~ treatment | originSite)

contrast_PD <- contrast(emm_PD, method = "trt.vs.ctrl", 
                       ref = "within_site_transplant")
pred_PD <- as.data.frame(contrast_PD)

pred_PD <- pred_PD %>%
  mutate(ymin = estimate - SE,
         ymax = estimate + SE)

# rename treatment comparisons
pred_PD <- pred_PD %>%
  mutate(contrast = recode(contrast,
                           "warmed_one_step - within_site_transplant" = "Warmed one",
                           "warmed_two_steps - within_site_transplant" = "Warmed two",
                           "cooled_one_step - within_site_transplant" = "Cooled one",
                           "cooled_two_steps - within_site_transplant" = "Cooled two"),
         contrast = factor(contrast,
                           levels = c("Cooled two", "Cooled one", "Warmed one", "Warmed two")))

#rename treatment origin sites
pred_PD <- pred_PD %>%
  mutate(originSite = recode(originSite,
                             "Upper Montane" = "Low elevation",
                             "Pfeiler" = "Mid elevation",
                             "Monument" = "High elevation"),
         originSite = factor(originSite,
                             levels = c("Low elevation", "Mid elevation", "High elevation")))

#reorder groups
pred_PD$originSite <- factor(pred_PD$originSite,
                            levels  = c("Low elevation",
                                        "Mid elevation",
                                        "High elevation"))

pred_PD$contrast <- factor(pred_PD$contrast,
                          levels  = c("Cooled two",
                                      "Cooled one",
                                      "Warmed one",
                                      "Warmed two"))
# add asterisks for significance 
pred_PD <- pred_PD %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  ))

## MPD ------
emm_MPD <- emmeans(model4_n, ~ treatment | originSite)

contrast_MPD <- contrast(emm_MPD, method = "trt.vs.ctrl", 
                        ref = "within_site_transplant")
pred_MPD <- as.data.frame(contrast_MPD)

pred_MPD <- pred_MPD %>%
  mutate(ymin = estimate - SE,
         ymax = estimate + SE)

# rename treatment comparisons
pred_MPD <- pred_MPD %>%
  mutate(contrast = recode(contrast,
                           "warmed_one_step - within_site_transplant" = "Warmed one",
                           "warmed_two_steps - within_site_transplant" = "Warmed two",
                           "cooled_one_step - within_site_transplant" = "Cooled one",
                           "cooled_two_steps - within_site_transplant" = "Cooled two"),
         contrast = factor(contrast,
                           levels = c("Cooled two", "Cooled one", "Warmed one", "Warmed two")))

#rename treatment origin sites
pred_MPD <- pred_MPD %>%
  mutate(originSite = recode(originSite,
                             "Upper Montane" = "Low elevation",
                             "Pfeiler" = "Mid elevation",
                             "Monument" = "High elevation"),
         originSite = factor(originSite,
                             levels = c("Low elevation", "Mid elevation", "High elevation")))

#reorder groups
pred_MPD$originSite <- factor(pred_MPD$originSite,
                             levels  = c("Low elevation",
                                         "Mid elevation",
                                         "High elevation"))

pred_MPD$contrast <- factor(pred_MPD$contrast,
                           levels  = c("Cooled two",
                                       "Cooled one",
                                       "Warmed one",
                                       "Warmed two"))
# add asterisks for significance 
pred_MPD <- pred_MPD %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  ))

## MNTD---------
emm_MNTD <- emmeans(model5_n, ~ treatment | originSite)

contrast_MNTD <- contrast(emm_MNTD, method = "trt.vs.ctrl", 
                         ref = "within_site_transplant")
pred_MNTD <- as.data.frame(contrast_MNTD)

pred_MNTD <- pred_MNTD %>%
  mutate(ymin = estimate - SE,
         ymax = estimate + SE)

# rename treatment comparisons
pred_MNTD <- pred_MNTD %>%
  mutate(contrast = recode(contrast,
                           "warmed_one_step - within_site_transplant" = "Warmed one",
                           "warmed_two_steps - within_site_transplant" = "Warmed two",
                           "cooled_one_step - within_site_transplant" = "Cooled one",
                           "cooled_two_steps - within_site_transplant" = "Cooled two"),
         contrast = factor(contrast,
                           levels = c("Cooled two", "Cooled one", "Warmed one", "Warmed two")))

#rename treatment origin sites
pred_MNTD <- pred_MNTD %>%
  mutate(originSite = recode(originSite,
                             "Upper Montane" = "Low elevation",
                             "Pfeiler" = "Mid elevation",
                             "Monument" = "High elevation"),
         originSite = factor(originSite,
                             levels = c("Low elevation", "Mid elevation", "High elevation")))

#reorder groups
pred_MNTD$originSite <- factor(pred_MNTD$originSite,
                              levels  = c("Low elevation",
                                          "Mid elevation",
                                          "High elevation"))

pred_MNTD$contrast <- factor(pred_MNTD$contrast,
                            levels  = c("Cooled two",
                                        "Cooled one",
                                        "Warmed one",
                                        "Warmed two"))
# add asterisks for significance 
pred_MNTD <- pred_MNTD %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  ))
