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
Anova(model1)
tab_model(model1)

#visualize random effects 
plotREsim(REsim(model1))

re.effects <- plot_model(model1, type = "re", show.values = TRUE)

plot(re.effects)

#test plotting

#make table of species abundance change
table <- get_model_data(model1, type = "re")

#save as csv
write.csv(table, file = "Data/Abundance_change.csv")

#check residuals
plot(fitted(model1), resid(model1, type = "pearson"))# this will create the plot
abline(0,0, col="red")
qqnorm(resid(model1))
qqline(resid(model1), col = "red")

##Species abundance changes based on range size
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
##Shannon diversity across years & tx---------------------------------
#bring in data
h_dat <- read.csv("Data/Shannon_fulldataset2018-2023.csv")

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

#model
model2 <- lmer(shannon_plots ~ year + treatment + originSite + 
                 (1|replicates), data = h_dat)
 
check_model(model2)
summary(model2)
Anova(model2)

#add model fits to dataframe to use in figure
h_dat$fit <- predict(model2)
write.csv(h_dat,"Data/h_dat.csv")

#save model2 output 
saveRDS(model2, file = "ModelOutput/Shannon_LMM.RDS")

#make figure 
shannon_output <- readRDS("ModelOutput/Shannon_LMM.RDS")

test <- ggpredict(shannon_output, terms = c("year", "treatment"))

test2 <- ggemmeans(shannon_output, terms = c("year", "treatment"))

test3 <- ggaverage(shannon_output, terms = c("year", "treatment"))

pred2 <- ggpredict(model2, terms = c("year","treatment"))
plot(pred2)

plot_model(model2,
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of year and treatment on Shannon diversity")


#test predictions
test4 <- test_predictions(shannon_output, terms = c("year","treatment","originSite")) #need to fix


##PD across treatment and years---------------------------------
#bring in data


#set up sum to zero contrast
pd_dat18to23$originSite <- as.factor(pd_dat18to23$originSite)
contrasts(pd_dat18to23$originSite) <- contr.sum(length(levels(pd_dat18to23$originSite)))

#reorder treatments

pd_dat18to23$treatment <- relevel(factor(pd_dat18to23$treatment),
                            ref = "within_site_transplant")

pd_dat18to23$year <- relevel(factor(pd_dat18to23$year),
                            ref = "2018")
hist(pd_dat18to23$pd.obs.z)

#model
model3 <- lmer(pd.obs.z ~ year + treatment + originSite + (1|replicates),
               data = pd_dat18to23)

check_model(model3)
summary(model3)
Anova(model3)

pred3 <- ggpredict(model3, terms = c("year", "treatment"))
plot(pred3)


#save model3 output 
saveRDS(model3, file = "ModelOutput/PD_LMM.RDS")

#make figure 
#bring in model results
pd_output <- readRDS("ModelOutput/PD_LMM.RDS")

#test predictions

test4 <- test_predictions(pd_output, terms = c("orginSite","treatment")) #need to fix

plot_model(model3,show.values=TRUE, show.p=TRUE,
                   title="Effect of year and treatment on PD")

##MPD across treatment and years---------------------------------------

#bring in data
mpd_dat <- read.csv("Data/MPD_byPlot18-23.csv")

hist(mpd_dat$mpd.obs.z)

#set up sum to zero contrast
mpd_dat$originSite <- as.factor(mpd_dat$originSite)
contrasts(mpd_dat$originSite) <- contr.sum(length(levels(mpd_dat$originSite)))

#reorder treatments

mpd_dat$treatment <- relevel(factor(mpd_dat$treatment),
                                  ref = "within_site_transplant")

mpd_dat$year <- relevel(factor(mpd_dat$year),
                             ref = "2018")

#model
model4 <- lmer(mpd.obs.z ~ year + treatment + originSite + (1|replicates),
               data = mpd_dat)

#check model
check_model(model4)
summary(model4)
Anova(model4)

#save model 4 output 
saveRDS(model4, file = "ModelOutput/MPD_LMM.RDS")

#visualize results

#bring in model results
mpd_output <- readRDS("ModelOutput/MPD_LMM.RDS")

pred4 <- ggpredict(model4, terms = c("treatment","originSite"))
plot(pred4)

test_mpd <- test_predictions(mpd_output, terms = c("orginSite","treatment"))

plot_model(model4,
           show.values=TRUE, show.p=TRUE,
           title="Effect of year and treatment on MPD")

##MNTD across treatment and years---------------------------------------

#bring in data
mntd_dat <- read.csv("Data/MNTD_byPlot18-23.csv")

hist(mntd_dat$mntd.obs.z)

#set up sum to zero contrast
mntd_dat$originSite <- as.factor(mntd_dat$originSite)
contrasts(mntd_dat$originSite) <- contr.sum(length(levels(mntd_dat$originSite)))

#reorder treatments

mntd_dat$treatment <- relevel(factor(mntd_dat$treatment),
                             ref = "within_site_transplant")

mntd_dat$year <- relevel(factor(mntd_dat$year),
                        ref = "2018")
#model
model5 <- lmer(mntd.obs.z ~ year + treatment + originSite + (1|replicates),
               data = mntd_dat)

#check model
check_model(model5)
summary(model5)
Anova(model5)

#save model output 
saveRDS(model5, file = "ModelOutput/MNTD_LMM.RDS")

#visualize results

#bring in model results
mntd_output <- readRDS("ModelOutput/MNTD_LMM.RDS")

pred5 <- ggpredict(model5, terms = c("originSite", "treatment"))
plot(pred5)

test_mntd <- test_predictions(mntd_output, terms = c("orginSite","treatment"))

plot_model(model5,
           show.values=TRUE, show.p=TRUE,type = "est",
           title="Effect of year and treatment on MNTD")


