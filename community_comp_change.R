#data wrangling and reformatting with Lorah's transplant data
#pivot wider to make comm phylo matrix 

install.packages("lme4") #modelling
install.packages("lmerTest") #provides p values
install.packages("performance") #model diagnostics
install.packages("ggeffects") #easy prediction & comparison

library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(ggeffects)

#bring in data
abundance_df <- read.csv("occurance2017-2023.csv")

#test model 
hist(abundance_df$occurrenceCount)

test <- lmer(occurrenceCount ~ year * treatment + originSite +(1|species),
     data = abundance_df)
summary(test)
