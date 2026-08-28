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

# TEST NEW FIGURES -------

## change over time with year on X ##############
# Get marginal predictions across year (factor), grouped by treatment, faceted by originSite
valid_combos <- h_dat %>%
  distinct(originSite, treatment)

preds_filtered <- preds %>%
  semi_join(valid_combos, by = c("group" = "treatment", "facet" = "originSite"))

# Get contrasts: treatment vs within_site_transplant, within each originSite AND year
emm_year <- emmeans(model_r, ~ treatment | originSite + year)

contrasts_year <- contrast(emm_year, method = "trt.vs.ctrl", 
                           ref = "within_site_transplant")

sig_df <- as.data.frame(contrasts_year) %>%
  # extract the "treatment" name from the contrast label, e.g. "cooled_one_step - within_site_transplant"
  mutate(treatment = gsub(" - within_site_transplant", "", contrast)) %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE ~ ""
  )) %>%
  filter(sig != "")   # keep only significant points to annotate

sig_df

sig_positions <- sig_df %>%
  left_join(
    preds_filtered %>% rename(treatment = group, originSite = facet, year = x),
    by = c("originSite", "year", "treatment")
  )

fig_withstars <- ggplot(preds_filtered, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(aes(group = group), linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, alpha = 0.6) +
  geom_text(
    data = sig_positions,
    aes(x = year, y = conf.high + 1, label = sig),   # nudge above the upper CI
    color = "black", size = 5, inherit.aes = FALSE
  ) +
  facet_wrap(~ facet) +
  scale_color_manual(
    values = c(
      "cooled_two_steps"       = "blue4",
      "cooled_one_step"        = "dodgerblue2",
      "within_site_transplant" = "grey35",
      "warmed_one_step"        = "orange1",
      "warmed_two_steps"       = "red3"
    ),
    labels = c(
      "cooled_two_steps"       = "Cooled two steps",
      "cooled_one_step"        = "Cooled one step",
      "within_site_transplant" = "Local transplant",
      "warmed_one_step"        = "Warmed one step",
      "warmed_two_steps"       = "Warmed two steps"
    )
  ) +
  labs(
    x = "Year",
    y = "Predicted species richness",
    color = "Treatment"
  ) +
  theme_bw(base_size = 20)

fig_withstars

fig_nostars <- ggplot(preds_filtered, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(aes(group = group), linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, alpha = 0.6) +
  facet_wrap(~ facet) +
  scale_color_manual(
    values = c(
      "cooled_two_steps"       = "blue4",
      "cooled_one_step"        = "dodgerblue2",
      "within_site_transplant" = "grey35",
      "warmed_one_step"        = "orange1",
      "warmed_two_steps"       = "red3"
    ),
    labels = c(
      "cooled_two_steps"       = "Cooled two steps",
      "cooled_one_step"        = "Cooled one step",
      "within_site_transplant" = "Local transplant",
      "warmed_one_step"        = "Warmed one step",
      "warmed_two_steps"       = "Warmed two steps"
    )
  ) +
  labs(
    x = "Year",
    y = "Predicted species richness",
    color = "Treatment"
  ) +
  theme_bw(base_size = 20)

fig_nostars

fig_withdata <- fig_nostars +
  geom_jitter(
    data = h_dat,
    aes(x = year, y = richness_df, color = treatment, group = replicates),
    width = 0.1, alpha = 0.3, inherit.aes = FALSE
  )
fig_withdata

## significance of variables ############
# Extract fixed effects with CIs
coefs <- tidy(model_r, effects = "fixed", conf.int = TRUE)

# Drop the intercept (usually not meaningful to plot alongside effect sizes)
coefs <- coefs %>% filter(term != "(Intercept)")

# Optional: clean up term labels for readability
coefs <- coefs %>%
  mutate(term = gsub("originSite", "Origin: ", term),
         term = gsub("treatment", "Treatment: ", term),
         term = gsub(":", " × ", term))

ggplot(coefs, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15) +
  geom_point(size = 3, color = "steelblue") +
  labs(
    x = "Estimate (95% CI)",
    y = NULL,
    title = "Fixed effects: species richness model"
  ) +
  theme_minimal(base_size = 13)

## comparison with within site transplannt #########

# Get estimated marginal means for treatment, within each origin site
emm <- emmeans(model_r, ~ treatment | originSite)

# Compare every other treatment level to "within_site_transplant" as reference,
# separately within each origin site
contrasts_r <- contrast(emm, method = "trt.vs.ctrl", ref = "within_site_transplant")

# Convert to a data frame with confidence intervals
contrast_df <- as.data.frame(confint(contrasts_r))

contrast_df

ggplot(contrast_df, aes(x = estimate, y = contrast)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.15) +
  geom_point(size = 3, color = "steelblue") +
  facet_wrap(~ originSite, scales = "free_y") +
  labs(
    x = "Difference in richness vs. within-site transplant (95% CI)",
    y = NULL,
    title = "Treatment effects relative to within-site transplant, by origin site"
  ) +
  theme_minimal(base_size = 13)
