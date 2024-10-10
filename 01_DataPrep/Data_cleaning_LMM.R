library(tidyverse)
library(dplyr)

#Make dataframe with Shannon values for linear models---------------------------

#bring in data
shannon_df_plotID <- read.csv("Data/Shannon_ByPlot2018-2023.csv")
abundance_df1 <- read.csv("Data/abundance_clean2018-2023.csv")

#get rid of extra column in abundance df
abundance_df1 = subset(abundance_df1, select = -c(X))

#get rid of extra control plots
outs <- c("untouched","within_site_transplant")

abundance_df1 <- abundance_df1 %>% filter(!treatment %in% outs)

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
abundance_df2 <- read.csv("Data/occurance2017-2023.csv")

# remove non-species from species column
gc.outs <- c("litter", "bare_soil", "rock")

#remove block 6 plots since they were transplanted in 2018
block.outs <- c("mo6-1", "mo6-2", "mo6-3", "mo6-4","mo6-5", "pf6-1",
                "pf6-2", "pf6-3","pf6-4", "um6-1", "um6-2","um6-3","um6-4",
                "um6-5","um6-6")


#filter data for things you never want
abundance_df2 <- abundance_df2 %>% filter(!is.na(treatment),
                                         !species %in% gc.outs,
                                         !originPlotID %in% block.outs,
                                         !treatment %in% outs)

#get rid of extra X columns
abundance_df2 = subset(abundance_df2, select = -c(X,X.1))

#get rid of multiple rows per plot
join_dat <- abundance_df2 %>% select(!8:13) %>% 
  group_by(turfID,year) %>%
  slice(1)

#rename columns to match abundance dataset
pd_df <- pd_df %>% 
  rename(
    treatmentOriginGroup = tx_site,
    originPlotID = plotID)

#get rid of merged column
pd_df = subset(pd_df, select = -c(X))

#merge dataframes
pd_dat <- left_join(pd_df, join_dat, by = c("turfID","originPlotID", "year","treatmentOriginGroup"))

#add column to ID replication
pd_dat$replicates <- paste(pd_dat$originSite,"_", pd_dat$destinationSite,"_",
                          pd_dat$treatment,"_", pd_dat$year)

#get rid of 2017
pd_dat18to23 <- pd_dat %>% filter(!year %in% 2017)

##MPD#########

#bring in data
mpd_df <- read.csv("Data/MPD_byPlot.csv")

#rename columns to match abundance dataset
mpd_df <- mpd_df %>% 
  rename(
    treatmentOriginGroup = tx_site,
    originPlotID = plotID)

#get rid of merged column
mpd_df = subset(mpd_df, select = -c(X))

#get rid of 2017
mpd_df <- mpd_df %>% filter(!year %in% 2017)

#merge dataframes
mpd_dat <- left_join(mpd_df, join_dat, by = c("turfID","originPlotID", "year",
                                              "treatmentOriginGroup"))

#add column to ID replication
mpd_dat$replicates <- paste(mpd_dat$originSite,"_", mpd_dat$destinationSite,"_",
                           mpd_dat$treatment,"_", mpd_dat$year)

#get rid of extra control treatments
mpd_dat <- mpd_dat %>% filter(!treatment %in% outs)
mpd_dat <- na.omit(mpd_dat)

#save as csv
write.csv(mpd_dat, file= "Data/MPD_byPlot18-23.csv")

##MNTD######
#bring in data
mntd_df <- read.csv("Data/MNTD_byPlot.csv")

#rename columns to match abundance dataset
mntd_df <- mntd_df %>% 
  rename(
    treatmentOriginGroup = tx_site,
    originPlotID = plotID)

#get rid of merged column
mntd_df = subset(mntd_df, select = -c(X))

#merge dataframes
mntd_dat <- left_join(mntd_df, join_dat, by = c("originPlotID", "year","treatmentOriginGroup"))

#add column to ID replication
mntd_dat$replicates <- paste(mntd_dat$originSite,"_", mntd_dat$destinationSite,"_",
                            mntd_dat$treatment,"_", mntd_dat$year)

#save as csv
write.csv(mntd_dat, file= "Data/MNTD_byPlot18-23.csv")

#Add range sizes into abundance data to use in linear modeling------------------

#log AOO vlaues
abundance_df1$AOO_log <-log(abundance_df1$AOO)


