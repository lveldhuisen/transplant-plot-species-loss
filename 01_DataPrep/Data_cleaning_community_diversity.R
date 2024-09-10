library(dplyr)
library(tidyverse)

#data cleaning for community diversity metrics

#Data cleaning and formatting---------------------------------------------------
#bring in data
abundance_df <- read.csv("occurance2017-2023.csv")
abundance_df$year <- as.factor(abundance_df$year)

#use only these years
ins <- c("2018","2019","2021","2022","2023")

#get rid of extra control plots
outs <- c("untouched","within_site_transplant")
ins_controlonly <- c("netted_untouched","within_site_transplant","untouched" )

# remove non-species from species column
gc.outs <- c("litter", "bare_soil", "rock")

#remove block 6 plots since they were transplanted in 2018
block.outs <- c("mo6-1", "mo6-2", "mo6-3", "mo6-4","mo6-5", "pf6-1",
                "pf6-2", "pf6-3","pf6-4", "um6-1", "um6-2","um6-3","um6-4",
                "um6-5","um6-6")

#filter data for things you never want
abundance_df1 <- abundance_df %>% filter(!is.na(treatment),
                                         !species %in% gc.outs,
                                         !originPlotID %in% block.outs,
                                         !treatment %in% outs,
                                         year %in% ins)
