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
library(patchwork)

# RICHNESS LMM TESTS ----------------------------

## change over time with years on X ----------

# Get marginal predictions across year (factor), grouped/colored by originSite, faceted by treatment
preds <- ggpredict(model_r, terms = c("year", "originSite", "treatment"))


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
  geom_line(aes(group = group), linewidth = 1.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, alpha = 0.6) +
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
    y = "Species richness",
    color = "Treatment"
  ) +
  theme_bw(base_size = 22)

overtime_nostars

# add raw data 

h_dat$facet <- as.character(h_dat$originSite)
h_dat$facet[h_dat$facet == 'Upper Montane'] <- 'Low elevation (2900 m)'
h_dat$facet[h_dat$facet == 'Pfeiler']       <- 'Mid elevation (3200 m)'
h_dat$facet[h_dat$facet == 'Monument']      <- 'High elevation (3300 m)'

# match factor order to preds_filtered so panels line up
h_dat$facet <- factor(h_dat$facet, levels = levels(preds_filtered$facet))

richness_fig_withdata <- overtime_nostars +
  geom_jitter(
    data = h_dat,
    aes(x = year, y = richness_df, group = replicates, color = treatment),
    width = 0.1, alpha = 0.2, inherit.aes = FALSE
  ) +
  facet_wrap(~ facet)

richness_fig_withdata

# SHANNON --------------------

# 1. Get marginal predictions across year, colored by originSite, faceted by treatment... 
# wait -- matching your richness figure: colored by treatment, faceted by originSite
preds2 <- ggpredict(model2_n, terms = c("year", "treatment", "originSite"))

# 2. Filter to only originSite x treatment combos that actually exist in the data
valid_combos <- h_dat %>% distinct(originSite, treatment)

preds2_filtered <- preds2 %>%
  semi_join(valid_combos, by = c("group" = "treatment", "facet" = "originSite"))

# 3. Relabel facets to elevation names, matching your richness figure
preds2_filtered$facet <- as.character(preds2_filtered$facet)
preds2_filtered$facet[preds2_filtered$facet == 'Upper Montane'] <- 'Low elevation (2900 m)'
preds2_filtered$facet[preds2_filtered$facet == 'Pfeiler']       <- 'Mid elevation (3200 m)'
preds2_filtered$facet[preds2_filtered$facet == 'Monument']      <- 'High elevation (3300 m)'

preds2_filtered$facet <- factor(
  preds2_filtered$facet,
  levels = c('Low elevation (2900 m)', 'Mid elevation (3200 m)', 'High elevation (3300 m)')
)

#reorder treatments and origin site
preds2_filtered$group <- factor(preds2_filtered$group, 
                               levels = c("cooled_two_steps",
                                          "cooled_one_step",
                                          "within_site_transplant",
                                          "warmed_one_step",
                                          "warmed_two_steps"))


overtime_shannon <- ggplot(preds2_filtered, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(aes(group = group), linewidth = 1.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, alpha = 0.6) +
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
    y = "Shannon diversity",
    color = "Treatment"
  ) +
  theme_bw(base_size = 22)

overtime_shannon

# match factor order to preds_filtered so panels line up
h_dat$facet <- factor(h_dat$facet, levels = levels(preds2_filtered$facet))

shannon_fig_withdata <- overtime_shannon +
  geom_jitter(
    data = h_dat,
    aes(x = year, y = shannon_plots, group = replicates, color = treatment),
    width = 0.1, alpha = 0.2, inherit.aes = FALSE
  ) +
  facet_wrap(~ facet)

shannon_fig_withdata

# EVENNESS ----------------------

# 1. Get marginal predictions across year, colored by originSite, faceted by treatment... 
# wait -- matching your richness figure: colored by treatment, faceted by originSite
preds6 <- ggpredict(model_6, terms = c("year", "treatment", "originSite"))

# 2. Filter to only originSite x treatment combos that actually exist in the data
valid_combos <- h_dat %>% distinct(originSite, treatment)

preds6_filtered <- preds6 %>%
  semi_join(valid_combos, by = c("group" = "treatment", "facet" = "originSite"))

# 3. Relabel facets to elevation names, matching your richness figure
preds6_filtered$facet <- as.character(preds6_filtered$facet)
preds6_filtered$facet[preds6_filtered$facet == 'Upper Montane'] <- 'Low elevation (2900 m)'
preds6_filtered$facet[preds6_filtered$facet == 'Pfeiler']       <- 'Mid elevation (3200 m)'
preds6_filtered$facet[preds6_filtered$facet == 'Monument']      <- 'High elevation (3300 m)'

preds6_filtered$facet <- factor(
  preds6_filtered$facet,
  levels = c('Low elevation (2900 m)', 'Mid elevation (3200 m)', 'High elevation (3300 m)')
)

#reorder treatments and origin site
preds6_filtered$group <- factor(preds6_filtered$group, 
                                levels = c("cooled_two_steps",
                                           "cooled_one_step",
                                           "within_site_transplant",
                                           "warmed_one_step",
                                           "warmed_two_steps"))


evenness_fig <- ggplot(preds6_filtered, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(aes(group = group), linewidth = 1.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, alpha = 0.6) +
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
    y = "Evenness",
    color = "Treatment"
  ) +
  theme_bw(base_size = 22)

evenness_fig

# match factor order to preds_filtered so panels line up
h_dat$facet <- factor(h_dat$facet, levels = levels(preds6_filtered$facet))

evenness_fig_withdata <- evenness_fig +
  geom_jitter(
    data = h_dat,
    aes(x = year, y = eveness, group = replicates, color = treatment),
    width = 0.1, alpha = 0.2, inherit.aes = FALSE
  ) +
  facet_wrap(~ facet)

evenness_fig_withdata

# COMBINE FOR FIG 2 --------

fig2 <- richness_fig_withdata / shannon_fig_withdata / evenness_fig_withdata + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = 'collect', axes = 'collect')
fig2
ggsave("Figures/fig2_sept26_revision.png", dpi = 600, height = 15, width = 15)


# PD --------------------

# 1. Get marginal predictions across year, colored by originSite, faceted by treatment... 
# wait -- matching your richness figure: colored by treatment, faceted by originSite
preds3 <- ggpredict(model3_n, terms = c("year", "treatment", "originSite"))

# 2. Filter to only originSite x treatment combos that actually exist in the data
valid_combos_pd <- pd_dat18to23 %>% distinct(originSite, treatment)
valid_combos_pd

preds3_filtered <- preds3 %>%
  semi_join(valid_combos, by = c("group" = "treatment", "facet" = "originSite"))



# 3. Relabel facets to elevation names, matching your richness figure
preds3_filtered$facet <- as.character(preds3_filtered$facet)
preds3_filtered$facet[preds3_filtered$facet == 'Upper Montane'] <- 'Low elevation (2900 m)'
preds3_filtered$facet[preds3_filtered$facet == 'Pfeiler']       <- 'Mid elevation (3200 m)'
preds3_filtered$facet[preds3_filtered$facet == 'Monument']      <- 'High elevation (3300 m)'

preds3_filtered$facet <- factor(
  preds3_filtered$facet,
  levels = c('Low elevation (2900 m)', 'Mid elevation (3200 m)', 'High elevation (3300 m)')
)

#reorder treatments and origin site
preds3_filtered$group <- factor(preds3_filtered$group, 
                                levels = c("cooled_two_steps",
                                           "cooled_one_step",
                                           "within_site_transplant",
                                           "warmed_one_step",
                                           "warmed_two_steps"))
pd_label <- c(expression(PD[SES]))

# make figure

pd_fig <- ggplot(preds3_filtered, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(aes(group = group), linewidth = 1.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, alpha = 0.6) +
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
    y = pd_label,
    color = "Treatment"
  ) +
  theme_bw(base_size = 22)

pd_fig

# add raw data 

pd_dat18to23$facet <- as.character(pd_dat18to23$originSite)
pd_dat18to23$facet[pd_dat18to23$facet == 'Upper Montane'] <- 'Low elevation (2900 m)'
pd_dat18to23$facet[pd_dat18to23$facet == 'Pfeiler']       <- 'Mid elevation (3200 m)'
pd_dat18to23$facet[pd_dat18to23$facet == 'Monument']      <- 'High elevation (3300 m)'

# match factor order to preds_filtered so panels line up
pd_dat18to23$facet <- factor(pd_dat18to23$facet, levels = levels(preds3_filtered$facet))

pd_fig_withdata <- pd_fig +
  geom_jitter(
    data = pd_dat18to23,
    aes(x = year, y = pd.obs.z, group = replicates, color = treatment),
    width = 0.1, alpha = 0.2, inherit.aes = FALSE
  ) +
  facet_wrap(~ facet)

pd_fig_withdata

# MPD --------------------
# 1. Get marginal predictions across year, colored by originSite, faceted by treatment... 
# wait -- matching your richness figure: colored by treatment, faceted by originSite
preds4 <- ggpredict(model4_n, terms = c("year", "treatment", "originSite"))

# 2. Filter to only originSite x treatment combos that actually exist in the data
valid_combos_mpd <- mpd_dat %>% distinct(originSite, treatment)
valid_combos_mpd

preds4_filtered <- preds4 %>%
  semi_join(valid_combos, by = c("group" = "treatment", "facet" = "originSite"))



# 3. Relabel facets to elevation names, matching your richness figure
preds4_filtered$facet <- as.character(preds4_filtered$facet)
preds4_filtered$facet[preds4_filtered$facet == 'Upper Montane'] <- 'Low elevation (2900 m)'
preds4_filtered$facet[preds4_filtered$facet == 'Pfeiler']       <- 'Mid elevation (3200 m)'
preds4_filtered$facet[preds4_filtered$facet == 'Monument']      <- 'High elevation (3300 m)'

preds4_filtered$facet <- factor(
  preds4_filtered$facet,
  levels = c('Low elevation (2900 m)', 'Mid elevation (3200 m)', 'High elevation (3300 m)')
)

#reorder treatments and origin site
preds4_filtered$group <- factor(preds4_filtered$group, 
                                levels = c("cooled_two_steps",
                                           "cooled_one_step",
                                           "within_site_transplant",
                                           "warmed_one_step",
                                           "warmed_two_steps"))
mpd_label <- c(expression(MPD[SES]))

# make figure

mpd_fig <- ggplot(preds4_filtered, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(aes(group = group), linewidth = 1.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, alpha = 0.6) +
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
    y = mpd_label,
    color = "Treatment"
  ) +
  theme_bw(base_size = 22)

mpd_fig

# add raw data 

mpd_dat$facet <- as.character(mpd_dat$originSite)
mpd_dat$facet[mpd_dat$facet == 'Upper Montane'] <- 'Low elevation (2900 m)'
mpd_dat$facet[mpd_dat$facet == 'Pfeiler']       <- 'Mid elevation (3200 m)'
mpd_dat$facet[mpd_dat$facet == 'Monument']      <- 'High elevation (3300 m)'

# match factor order to preds_filtered so panels line up
mpd_dat$facet <- factor(mpd_dat$facet, levels = levels(preds4_filtered$facet))

mpd_fig_withdata <- mpd_fig +
  geom_jitter(
    data = mpd_dat,
    aes(x = year, y = mpd.obs.z, group = replicates, color = treatment),
    width = 0.1, alpha = 0.2, inherit.aes = FALSE
  ) +
  facet_wrap(~ facet)

mpd_fig_withdata

# MNTD ---------------
# 1. Get marginal predictions across year, colored by originSite, faceted by treatment... 
# wait -- matching your richness figure: colored by treatment, faceted by originSite
preds5 <- ggpredict(model5_n, terms = c("year", "treatment", "originSite"))

# 2. Filter to only originSite x treatment combos that actually exist in the data
valid_combos_mntd <- mntd_dat %>% distinct(originSite, treatment)
valid_combos_mntd

preds5_filtered <- preds5 %>%
  semi_join(valid_combos, by = c("group" = "treatment", "facet" = "originSite"))



# 3. Relabel facets to elevation names, matching your richness figure
preds5_filtered$facet <- as.character(preds5_filtered$facet)
preds5_filtered$facet[preds5_filtered$facet == 'Upper Montane'] <- 'Low elevation (2900 m)'
preds5_filtered$facet[preds5_filtered$facet == 'Pfeiler']       <- 'Mid elevation (3200 m)'
preds5_filtered$facet[preds5_filtered$facet == 'Monument']      <- 'High elevation (3300 m)'

preds5_filtered$facet <- factor(
  preds5_filtered$facet,
  levels = c('Low elevation (2900 m)', 'Mid elevation (3200 m)', 'High elevation (3300 m)')
)

#reorder treatments and origin site
preds5_filtered$group <- factor(preds5_filtered$group, 
                                levels = c("cooled_two_steps",
                                           "cooled_one_step",
                                           "within_site_transplant",
                                           "warmed_one_step",
                                           "warmed_two_steps"))
mntd_label <- c(expression(MNTD[SES]))

# make figure

mntd_fig <- ggplot(preds5_filtered, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(aes(group = group), linewidth = 1.5) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.25, alpha = 0.6) +
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
    y = mntd_label,
    color = "Treatment"
  ) +
  theme_bw(base_size = 22)

mntd_fig

# add raw data 

mntd_dat$facet <- as.character(mntd_dat$originSite)
mntd_dat$facet[mntd_dat$facet == 'Upper Montane'] <- 'Low elevation (2900 m)'
mntd_dat$facet[mntd_dat$facet == 'Pfeiler']       <- 'Mid elevation (3200 m)'
mntd_dat$facet[mntd_dat$facet == 'Monument']      <- 'High elevation (3300 m)'

# match factor order to preds_filtered so panels line up
mntd_dat$facet <- factor(mntd_dat$facet, levels = levels(preds5_filtered$facet))

mntd_fig_withdata <- mntd_fig +
  geom_jitter(
    data = mntd_dat,
    aes(x = year, y = mntd.obs.z, group = replicates, color = treatment),
    width = 0.1, alpha = 0.2, inherit.aes = FALSE
  ) +
  facet_wrap(~ facet)

mntd_fig_withdata

# COMBINE FOR FIG 3 --------

fig3 <- pd_fig_withdata / mpd_fig_withdata / mntd_fig_withdata + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = 'collect', axes = 'collect')
fig3
ggsave("Figures/fig3_sept26_revision.png", dpi = 600, height = 15, width = 15)

# EVENESS SANITY CHECK ----

# Get estimated marginal means for treatment, within each origin site
emm <- emmeans(model_6, ~ treatment | originSite)

# Compare every other treatment level to "within_site_transplant" as reference,
# separately within each origin site
contrasts_e <- contrast(emm, method = "trt.vs.ctrl", ref = "within_site_transplant")

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
