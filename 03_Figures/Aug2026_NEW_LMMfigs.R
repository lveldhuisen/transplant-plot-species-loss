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

# RICHNESS LMM TESTS ----------------------------

## change over time with years on X ----------

# Get marginal predictions across year (factor), grouped by treatment, faceted by originSite
valid_combos <- h_dat %>%
  distinct(originSite, treatment)

valid_combos
preds_filtered <- preds %>%
  semi_join(valid_combos, by = c("group" = "treatment", "facet" = "originSite"))

#replace site names with elevations
preds_filtered$facet <- as.character(preds_filtered$facet)
preds_filtered$facet[preds_filtered$facet == 'Upper Montane'] <- 'Low elevation (2900 m)'
preds_filtered$facet[preds_filtered$facet == 'Pfeiler'] <- 'Mid elevation (3200 m)'
preds_filtered$facet[preds_filtered$facet == 'Monument'] <- 'High elevation (3300 m)'

#figures to display raw slope values----------

#reorder treatments and origin site
preds_filtered$group <- factor(preds_filtered$group, 
                              levels = c("cooled_two_steps",
                                         "cooled_one_step",
                                         "within_site_transplant",
                                         "warmed_one_step",
                                         "warmed_two_steps"))

preds_filtered$facet <- factor(preds_filtered$facet, 
                               levels = c("Low elevation (2900 m)",
                                          "Mid elevation (3200 m)",
                                          "High elevation (3300 m)"))

# make figure

overtime_nostars <- ggplot(preds_filtered, aes(x = x, y = predicted, color = group, group = group)) +
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

overtime_nostars

# add raw data 

h_dat$facet <- as.character(h_dat$originSite)
h_dat$facet[h_dat$facet == 'Upper Montane'] <- 'Low elevation (2900 m)'
h_dat$facet[h_dat$facet == 'Pfeiler']       <- 'Mid elevation (3200 m)'
h_dat$facet[h_dat$facet == 'Monument']      <- 'High elevation (3300 m)'

# match factor order to preds_filtered so panels line up
h_dat$facet <- factor(h_dat$facet, levels = levels(preds_filtered$facet))

fig_withdata <- overtime_nostars +
  geom_jitter(
    data = h_dat,
    aes(x = year, y = richness_df, group = replicates, color = "lightgrey"),
    width = 0.1, alpha = 0.1, inherit.aes = FALSE
  ) +
  facet_wrap(~ facet)

fig_withdata

## figure with significance asterisks ------------
# prep comparison with w/in site for significance asterisks

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

## comparison with within site transplant #########

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
  theme_bw(base_size = 13)
