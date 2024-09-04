#data wrangling and reformatting with Lorah's transplant data
#pivot wider to make comm phylo matrix 

install.packages("lme4") #modelling
install.packages("lmerTest") #provides p values
install.packages("performance") #model diagnostics
install.packages("ggeffects") #easy prediction & comparison
install.packages("sjPlot") #visualize random effects
install.packages("vegan") #community diversity
install.packages("stringr") #to remove spaces

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
outs <- c("untouched","within site transplant")

# remove non-species from species column
gc.outs <- c("litter", "bare soil", "rock")

#remove block 6 plots since they were transplanted in 2018
block.outs <- c("mo6-1", "mo6-2", "mo6-3", "mo6-4","mo6-5", "pf6-1",
                "pf6-2", "pf6-3","pf6-4", "um6-1", "um6-2","um6-3","um6-4",
                "um6-5","um6-6")

#filter data for things you never want
abundance_df1 <- abundance_df %>% filter(!is.na(treatment),
                                         !species %in% gc.outs)
abundance_df1 <- abundance_df1 %>% filter(!originPlotID %in% block.outs)

#reorder treatments
abundance_df1$treatment <- relevel(factor(abundance_df1$treatment),
                                   ref = "netted_untouched")
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

#remove other additional columns
comm_matrix = subset(comm_matrix, select = -c(X, X.1) )

#switch plot IDs to row names
comm_matrix1 <- comm_matrix %>%
  group_by(tx_year) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )
comm_matrix1 <- comm_matrix1 %>% column_to_rownames(var = "tx_year")


#calculate shannon diversity
shannon <- diversity(comm_matrix1, index = "shannon")

#make dataframe and reformat to use for plot
shannon_df <- as.data.frame(shannon)
shannon_df$tx_year <- row.names(shannon_df)

shannon_df <- shannon_df %>%
  separate(col = tx_year, into = c("tx_site", "year"), sep = " _ ")

#plot
shannon_fig <- ggplot(data = shannon_df, aes(x=year, y=shannon))+
  geom_boxplot()+
  facet_wrap(.~ tx_site)+
    theme_bw()
plot(shannon_fig)

#calculate simpson's diversity 
simpson <- diversity(comm_matrix1, index = "simpson")

#make dataframe and reformat to use for plot
simpson_df <- as.data.frame(simpson)
simpson_df$tx_year <- row.names(simpson_df)

simpson_df <- simpson_df %>%
  separate(col = tx_year, into = c("tx_site", "year"), sep = " _ ")

#plot
simpson_fig <- ggplot(data = simpson_df, aes(x=year, y=simpson))+
  geom_boxplot()+
  facet_wrap(.~ tx_site)+
  theme_bw()
plot(simpson_fig)
             