library(dplyr)
library(tidyverse)

#Data formatting--------------

#bring in data
abundance_df <- read.csv("Data/occurance2017-2023.csv")
abundance_df$year <- as.factor(abundance_df$year)