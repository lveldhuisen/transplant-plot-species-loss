#data wrangling and reformatting with Lorah's transplant data
#pivot wider to make comm phylo matrix 

install.packages("lme4") #modelling
install.packages("lmerTest") #provides p values
install.packages("performance") #model diagnostics
install.packages("ggeffects") #easy prediction & comparison
install.packages("sjPlot") #visualize random effects
install.packages("vegan") #community diversity

library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(ggeffects)
library(sjPlot)
library(vegan)

#bring in data
abundance_df <- read.csv("occurance2017-2023.csv")
abundance_df$year <- as.factor(abundance_df$year)

#use only these years
ins <- c("2018","2023")

#get rid of extra control plots
outs <- c("untouched","within site transplant")
# remove non-species from species column
gc.outs <- c("litter", "bare soil", "rock")

#filter data for things you never want
abundance_df1 <- abundance_df %>% filter(!is.na(treatment),
                                         !species %in% gc.outs)
#reorder treatments

abundance_df1$treatment <- relevel(factor(abundance_df1$treatment),
                                   ref = "netted untouched")
#test model 
hist(abundance_df$occurrenceCount)

#set up sum to zero contrast
abundance_df$originSite <- as.factor(abundance_df1$originSite)
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

#calculate diversity metrics for each site and treatment using vegan 

#reformat data
comm_matrix <- pivot_wider(abundance_df1, names_from = species, values_from = occurrenceCount)

#remove extra columns 
comm_matrix = subset(comm_matrix, select = -c(turfID, originSite, destinationSite,
                                              originPlotID, treatment, date_yyyymmdd, functionalGroup, unknownMorpho, percentCover) )

#add column for year and treatment
comm_matrix$tx_year = NA
comm_matrix$tx_year <- paste(comm_matrix$treatmentOriginGroup, "_",comm_matrix$year)
comm_matrix <- comm_matrix %>% relocate(tx_year)
comm_matrix = subset(comm_matrix, select = -c(year, treatmentOriginGroup) )

#replace NAs with 0s
comm_matrix[is.na(comm_matrix)] <- 0

#switch plot IDs to row names
comm_matrix1 <- comm_matrix %>%
  group_by(tx_year) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )


#calculate diversity
diversity(comm_matrix1, index = "shannon", groups, equalize.groups = FALSE, MARGIN = 1, base = exp(1))
