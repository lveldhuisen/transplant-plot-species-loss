#data wrangling and reformatting with Lorah's transplant data
#pivot wider to make comm phylo matrix 

install.packages("lme4") #modelling
install.packages("lmerTest") #provides p values
install.packages("performance") #model diagnostics
install.packages("ggeffects") #easy prediction & comparison
install.packages("sjPlot") #visualize random effects

library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(ggeffects)
library(sjPlot)

#bring in data
abundance_df <- read.csv("occurance2017-2023.csv")

#test model 
hist(abundance_df$occurrenceCount)

model1 <- lmer(occurrenceCount ~ year * treatment + originSite + (1|species),
     data = abundance_df)
summary(model1)

(re.effects <- plot_model(model1, type = "re", show.values = TRUE))
plot(re.effects)

ggplot(abundance_df, aes(x = year, y = occurenceCount, colour = treatment)) +
  facet_wrap(~originSite, nrow=2) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(abundance_df, pred = predict(model1)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  
