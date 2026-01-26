#linear mixed models to see which species are changing and how, also includes
#changes in shannon diversity and phylogenetic diversity 

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

#Species-level changes-----------------

##Predict abundance numbers across years & tx--------------------------
#bring in data
abundance_df1 <- read.csv("Data/abundance_clean2018-2023.csv")
abundance_df1$year <- as.factor(abundance_df1$year)

#use only within site transplant for control
control.outs <- c("netted_untouched","untouched")
abundance_df1 <- abundance_df1 %>% filter(!is.na(treatment),
                                          !treatment %in% control.outs)

#reorder treatments
abundance_df1$treatment <- relevel(factor(abundance_df1$treatment),
                                   ref = "within_site_transplant")
abundance_df1$year <- relevel(factor(abundance_df1$year),
                                   ref = "2018")

hist(abundance_df1$occurrenceCount)

#set up sum to zero contrast
abundance_df1$originSite <- as.factor(abundance_df1$originSite)
contrasts(abundance_df1$originSite) <- contr.sum(length(levels(abundance_df1$originSite)))

#model
model1 <- lmer(log1p(occurrenceCount) ~ year + treatment + originSite +
                 (1+ treatment|species), data = abundance_df1, REML = FALSE)

#check model diagnostics before you look at summary. Is this model fucked?
check_model(model1)

#see model summary
summary(model1)
ranova(model1)
anova(model1)
tab_model(model1)

#visualize random effects 
plotREsim(REsim(model1))

re.effects <- plot_model(model1, type = "re", show.values = TRUE, show.p = TRUE)

plot(re.effects)

#test plotting
plot_model(model1,
           show.values=TRUE, show.p=TRUE,
           title="Effect of")

#make table of species abundance change
table <- get_model_data(model1, type = "re")
write.csv(table, file = "Data/Abundance_change.csv")

#check residuals
plot(fitted(model1), resid(model1, type = "pearson"))# this will create the plot
abline(0,0, col="red")
qqnorm(resid(model1))
qqline(resid(model1), col = "red")

##Species abundance changes based on range size#####
#model
model_aoo <- lmer(log1p(occurrenceCount) ~ year + treatment + originSite +
               log(AOO)  + (1|species), data = abundance_df1, REML = FALSE)

#check model diagnostics before you look at summary. Is this model fucked?
check_model(model_aoo)

#see model summary
summary(model_aoo)
ranova(model_aoo)
tab_model(model_aoo)

#visualize random effects 
plotREsim(REsim(model_aoo))

re.effects <- plot_model(model_aoo, type = "re", show.values = TRUE)

plot(re.effects)

#Community-level changes-----------------------

#tested for interaction between year and treatment, was not significant
#cooled two steps had marginal significance in 2022 and 2023
#anova showed interaction didnt significantly improve model performance

##Richness across treatment and years--------------

###bring in data####
h_dat <- read.csv("Data/Shannon_fulldataset2018-2023.csv")

#use only within site transplant for control
control.outs <- c("netted_untouched","untouched")
h_dat <- h_dat %>% filter(!is.na(treatment),
                          !treatment %in% control.outs)

#reorder treatments
h_dat$treatment <- relevel(factor(h_dat$treatment),
                           ref = "within_site_transplant")

h_dat$year <- relevel(factor(h_dat$year),
                      ref = "2018")

#remove extra column
h_dat = subset(h_dat, select = -c(X.1))

#set up sum to zero contrast
h_dat$originSite <- as.factor(h_dat$originSite)
contrasts(h_dat$originSite) <- contr.sum(length(levels(h_dat$originSite)))

# make column for block
h_dat <- h_dat %>%
  separate(turfID,
           into = c("origin_block", "o_blockplot", "tx", "dest_block","d_blockplot"),
           sep = "[-_]")

###model nested#####
model_r <- lmer(richness_df ~ year + originSite/treatment + 
                  (1|replicates), data = h_dat)

check_model(model_r)
summary(model_r)
Anova(model_r)
AIC(model_r)

###model nested with random block effect#####
model_r_b <- lmer(richness_df ~ year + originSite/treatment + 
                  (1|replicates) + (1|origin_block), data = h_dat)

check_model(model_r_b)
summary(model_r_b)
Anova(model_r_b)


###model additive only#####
model_r1 <- lmer(richness_df ~ year + originSite + treatment + 
                   (1|replicates), data = h_dat)

summary(model_r1)
AIC(model_r1,model_r)

###compare nested and nonnested models####
compare_performance(model_r,model_r1, model_r_b, rank = T) #nested looks better

#save nested output
pred_R <- test_predictions(model_r, terms = c("originSite","treatment"))

#save as csv
write_csv(pred_R, file = "ModelOutput/Prediction_richness_nested.csv")

#save model2 output 
saveRDS(model_r, file = "ModelOutput/Richness_LMM.RDS")

##Shannon diversity across years & tx---------------------------------
shannon_df_plot_ID <- read.csv("Data/Shannon_cover_forLMM.csv")

###bring in and set up data###
h_dat <- shannon_df_plotID
h_dat$shannon_plots <- as.numeric(h_dat$shannon_plots)

#reorder treatments
h_dat$treatment <- relevel(factor(h_dat$treatment),
                           ref = "within_site_transplant")

h_dat$year <- relevel(factor(h_dat$year),
                           ref = "2018")

#set up sum to zero contrast
h_dat$originSite <- as.factor(h_dat$originSite)
contrasts(h_dat$originSite) <- contr.sum(length(levels(h_dat$originSite)))

# make column for block
h_dat <- h_dat %>%
  separate(turfID,
           into = c("origin_block", "o_blockplot", "tx", "dest_block","d_blockplot"),
           sep = "[-_]")

#add column to ID replication
h_dat$replicates <- paste(h_dat$originSite,"_", h_dat$destinationSite,"_",
                          h_dat$treatment,"_", h_dat$year)

###model nested#####
model2_n <- lmer(shannon_plots ~ year + 
                 originSite/treatment  + (1|replicates), data = h_dat)
 
check_model(model2_n)
summary(model2_n)
anova(model2_n)
AIC(model2_n)

### model nested with block random effect ####

model2_n_b <- lmer(shannon_plots ~ year + 
                   originSite/treatment  + (1|replicates) + (1|origin_block), data = h_dat)

check_model(model2_n_b)
summary(model2_n_b)

###model additive only#####
model2_a <- lmer(shannon_plots ~ year + 
                   originSite + treatment  + (1|replicates), data = h_dat)
check_model(model2_a)
AIC(model2_a)

###compare models####
compare_performance(model2_a,model2_n,model2_n_b, rank = T) #nested looks better

#save model2 output 
saveRDS(model2_n, file = "ModelOutput/Shannon_LMM.RDS")

#test predictions
prediction_shannon_nested <- test_predictions(model2_n, terms = c("originSite","treatment"))
prediction_shannon_nested_b <- test_predictions(model2_n_b, terms = c("originSite","treatment"))

#save as csv
write_csv(prediction_shannon_nested, file = "ModelOutput/Prediction_Shannon_nested.csv")

##PD across treatment and years---------------------------------
###bring in and set up data####
pd_dat18to23 <- pd_dat18to23 %>% filter(!is.na(treatment),
                                          !treatment %in% control.outs)
#set up sum to zero contrast
pd_dat18to23$originSite <- as.factor(pd_dat18to23$originSite)
contrasts(pd_dat18to23$originSite) <- contr.sum(length(levels(pd_dat18to23$originSite)))

#reorder treatments

pd_dat18to23$treatment <- relevel(factor(pd_dat18to23$treatment),
                            ref = "within_site_transplant")

pd_dat18to23$year <- relevel(factor(pd_dat18to23$year),
                            ref = "2018")
hist(pd_dat18to23$pd.obs.z)

# make column for block <- 
pd_dat18to23 <- pd_dat18to23 %>%
  separate(originPlotID,
           into = c("origin_block", "o_blockplot"),
           sep = "-")

###model not nested####
model3_a <- lmer(pd.obs.z ~ year + treatment + originSite + (1|replicates),
               data = pd_dat18to23)

check_model(model3_a)
summary(model3_a)
anova(model3_a)

###model nested####
model3_n <- lmer(pd.obs.z ~ year + originSite/treatment + (1|replicates),
                 data = pd_dat18to23)

###model nested with block effect #####
model3_n_b <- lmer(pd.obs.z ~ year + originSite/treatment + (1|replicates) + (1|origin_block),
                 data = pd_dat18to23)

###compare models####
AIC(model3_a)
AIC(model3_n)
compare_performance(model3_a, model3_n, model3_n_b, rank = T) #use nested

#save model3 output 
saveRDS(model3_n, file = "ModelOutput/PD_LMM.RDS")

#test predictions
predictions_pd_nested <- test_predictions(model3_n, terms = c("originSite","treatment"))
predictions_pd_nested_b <- test_predictions(model3_n_b, terms = c("originSite","treatment"))

#save as csv
write_csv(predictions_pd_nested, file = "ModelOutput/Prediction_pd_nested.csv")

##MPD across treatment and years---------------------------------------

###bring in and set up data####
mpd_dat <- read.csv("Data/MPD_byPlot18-23.csv")

mpd_dat <- mpd_dat %>% filter(!is.na(treatment),
                                        !treatment %in% control.outs)

hist(mpd_dat$mpd.obs.z)

#set up sum to zero contrast
mpd_dat$originSite <- as.factor(mpd_dat$originSite)
contrasts(mpd_dat$originSite) <- contr.sum(length(levels(mpd_dat$originSite)))

#reorder treatments

mpd_dat$treatment <- relevel(factor(mpd_dat$treatment),
                                  ref = "within_site_transplant")

mpd_dat$year <- relevel(factor(mpd_dat$year),
                             ref = "2018")

# make column for block <- 
mpd_dat <- mpd_dat %>%
  separate(originPlotID,
           into = c("origin_block", "o_blockplot"),
           sep = "-")

###model additive####
model4_a <- lmer(mpd.obs.z ~ year + treatment + originSite + (1|replicates),
               data = mpd_dat)

#check model
check_model(model4_a)
summary(model4_a)
anova(model4_a)

###model nested####
model4_n <- lmer(mpd.obs.z ~ year + originSite/treatment + (1|replicates),
                 data = mpd_dat)

# model with block effect 
###model nested with block effect #####
model4_n_b <- lmer(mpd.obs.z ~ year + originSite/treatment + (1|replicates) + (1|origin_block),
                   data = mpd_dat)

#compare nested and additive
compare_performance(model4_a, model4_n, model4_n_b, rank = T) #equal performance, use nested

#save model 4 output 
saveRDS(model4_n, file = "ModelOutput/MPD_LMM.RDS")

#test predictions
pred_mpd_nested <- test_predictions(model4_n, terms = c("originSite","treatment"))
pred_mpd_b <- test_predictions(model4_n_b, terms = c("originSite","treatment"))

#save as csv
write_csv(pred_mpd_nested, file = "ModelOutput/Prediction_mpd_nested.csv")

##MNTD across treatment and years---------------------------------------

###bring in and set up data####
mntd_dat <- read.csv("Data/MNTD_byPlot18-23.csv")

mntd_dat <- mntd_dat %>% filter(!treatment %in% control.outs)

hist(mntd_dat$mntd.obs.z)

#set up sum to zero contrast
mntd_dat$originSite <- as.factor(mntd_dat$originSite)
contrasts(mntd_dat$originSite) <- contr.sum(length(levels(mntd_dat$originSite)))

#reorder treatments

mntd_dat$treatment <- relevel(factor(mntd_dat$treatment),
                             ref = "within_site_transplant")

mntd_dat$year <- relevel(factor(mntd_dat$year),
                        ref = "2018")
###model additive####
model5_a <- lmer(mntd.obs.z ~ year + treatment + originSite + (1|replicates),
               data = mntd_dat)

#check model
check_model(model5_a)
summary(model5_a)
anova(model5_a)

###model nested####
model5_n <- lmer(mntd.obs.z ~ year + originSite/treatment + (1|replicates),
                 data = mntd_dat)

###compare nested and non-nested####
compare_performance(model5_a, model5_n, rank = T) #equal, use nested

#save model output 
saveRDS(model5_n, file = "ModelOutput/MNTD_LMM.RDS")

#test predictions
pred_mntd_nested <- test_predictions(model5_n, terms = c("originSite","treatment"))

#save as csv
write_csv(pred_mntd_nested, file = "ModelOutput/Prediction_mntd_nested.csv")

