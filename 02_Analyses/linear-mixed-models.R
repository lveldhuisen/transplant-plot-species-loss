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

#Model to predict abundance numbers across years & tx--------------------------
#bring in data
abundance_df1 <- read.csv("Data/abundance_clean2018-2023.csv")
abundance_df1$year <- as.factor(abundance_df$year)

#reorder treatments
abundance_df1$treatment <- relevel(factor(abundance_df1$treatment),
                                   ref = "netted_untouched")
#test model 
hist(abundance_df1$occurrenceCount)

#set up sum to zero contrast
abundance_df1$originSite <- as.factor(abundance_df1$originSite)
contrasts(abundance_df1$originSite) <- contr.sum(length(levels(abundance_df1$originSite)))

#model
model1 <- lmer(log1p(occurrenceCount) ~ year + treatment + originSite + 
                 (treatment|species), 
               data = abundance_df1 %>% filter(year %in% ins & 
                                                 !treatment %in% outs))

#check model diagnostics before you look at summary. Is this model fucked?
check_model(model1)

#see model summary
summary(model1)

#visualize random effects 
(re.effects <- plot_model(model1, type = "re", show.values = TRUE))

plot(re.effects)

#Model for shannon diversity across years & tx---------------------------------
#bring in data
h_dat <- read.csv("Data/Shannon_fulldataset2018-2023.csv")

#reorder treatments
h_dat$treatment <- relevel(factor(h_dat$treatment),
                           ref = "netted_untouched")

#remove extra column
h_dat = subset(h_dat, select = -c(X.1))

#model
model2 <- lmer(shannon_plots ~ year + treatment + originSite + (1|replicates),
               data = h_dat)

check_model(model2)
summary(model2)
Anova(model2)

#save model2 output 
saveRDS(model2, file = "ModelOutput/Shannon_LMM.RDS")

#make figure 
shannon_output <- readRDS("ModelOutput/Shannon_LMM.RDS")

test <- ggpredict(shannon_output, terms = c("year", "treatment"))

test2 <- ggemmeans(shannon_output, terms = c("year", "treatment"))

test3 <- ggaverage(shannon_output, terms = c("year", "treatment"))

pred2 <- ggpredict(model2, terms = c("year","treatment", "originSite"))
plot(pred2)


#test predictions
test4 <- test_predictions(shannon_output, terms = c("year","treatment","originSite")) #need to fix


#Model for phylognetic diversity across years & tx---------------------------------
#bring in data

#reorder treatments

pd_dat18to23$treatment <- relevel(factor(pd_dat18to23$treatment),
                            ref = "netted_untouched")

pd_dat18to23$year <- relevel(factor(pd_dat18to23$year),
                            ref = "2018")
hist(pd_dat18to23$pd.obs.z)

#model
model3 <- lmer(pd.obs.z ~ year + treatment + originSite + (1|replicates),
               data = pd_dat18to23)

check_model(model3)
summary(model3)
Anova(model3)

pred3 <- ggpredict(model3, terms = c("originSite", "treatment"))
plot(pred3)


#save model3 output 
saveRDS(model3, file = "ModelOutput/PD_LMM.RDS")

#make figure 
#bring in model results
pd_output <- readRDS("ModelOutput/PD_LMM.RDS")

#test predictions
test4 <- test_predictions(pd_output) #need to fix
