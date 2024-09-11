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

#Make dataframe with phylogenetic metrics for linear models---------------------

##PD######

#bring in data
pd_df <- read.csv("Data/PD_byPlot.csv")

#rename columns to match abundance dataset
pd_df <- pd_df %>% 
  rename(
    treatmentOriginGroup = tx_site,
    originPlotID = plotID)

#get rid of merged column
pd_df = subset(pd_df, select = -c(X))

#merge dataframes
pd_dat <- left_join(pd_df, join_dat, by = c("originPlotID", "year","treatmentOriginGroup"))

#add column to ID replication
pd_dat$replicates <- paste(pd_dat$originSite,"_", pd_dat$destinationSite,"_",
                          pd_dat$treatment,"_", pd_dat$year)




