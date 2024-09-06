#linear mixed models to see which species are changing and how

library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(ggeffects)
library(sjPlot)
library(vegan)
library(stringr)
library(dplyr)

#bring in data
abundance_df <- read.csv("occurance2017-2023.csv")
abundance_df$year <- as.factor(abundance_df$year)

#use only these years
ins <- c("2018","2023")

#get rid of extra control plots
outs <- c("untouched","within_site_transplant")

# remove non-species from species column
gc.outs <- c("litter", "bare_soil", "rock")

#remove block 6 plots since they were transplanted in 2018
block.outs <- c("mo6-1", "mo6-2", "mo6-3", "mo6-4","mo6-5", "pf6-1",
                "pf6-2", "pf6-3","pf6-4", "um6-1", "um6-2","um6-3","um6-4",
                "um6-5","um6-6")

#filter data for things you never want
abundance_df1 <- abundance_df %>% filter(!is.na(treatment),
                                         !species %in% gc.outs,
                                         !treatment %in% outs,
                                         !originPlotID %in% block.outs)

#reorder treatments
abundance_df1$treatment <- relevel(factor(abundance_df1$treatment),
                                   ref = "netted_untouched")
#test model 
hist(abundance_df$occurrenceCount)

#set up sum to zero contrast
abundance_df1$originSite <- as.factor(abundance_df1$originSite)
contrasts(abundance_df1$originSite) <- contr.sum(length(levels(abundance_df1$originSite)))

#model
model1 <- lmer(log1p(occurrenceCount) ~ year + treatment + originSite + 
                 (treatment|species), 
               data = abundance_df1 %>% filter(year %in% ins & 
                                                 !treatment %in% outs))
#model for shannon diversity
model2 <- lmer(shannon_plots ~ year + treatment + originSite + (1|replicates),
               data = h_dat)

check_model(model2)

#check model diagnostics before you look at summary. Is this model fucked?
check_model(model1)

#see model summary
summary(model1)

#visualize random effects 
(re.effects <- plot_model(model1, type = "re", show.values = TRUE))

plot(re.effects)