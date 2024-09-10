library(tidyverse)
library(dplyr)

#Make dataframe with Shannon values for linear models---------------------------

#bring in data
shannon_df_plotID <- read.csv("Data/Shannon_ByPlot2018-2023.csv")
abundance_df1 <- read.csv("Data/abundance_clean2018-2023.csv")

#get rid of extra column in abundance df
abundance_df1 = subset(abundance_df1, select = -c(X))

#get rid of multiple rows per plot
join_dat <- abundance_df1 %>% select(!8:13) %>% 
  group_by(originPlotID, treatmentOriginGroup, year) %>%
  slice(1)

#merge dataframes
h_dat <- left_join(shannon_df_plotID, join_dat, by = c("turfID",
                                                       "year","treatmentOriginGroup"))

#add column to ID replication
h_dat$replicates <- paste(h_dat$originSite,"_", h_dat$destinationSite,"_",
                          h_dat$treatment,"_", h_dat$year)

#save dataset
write.csv(h_dat, file = "Data/Shannon_fulldataset2018-2023.csv")
