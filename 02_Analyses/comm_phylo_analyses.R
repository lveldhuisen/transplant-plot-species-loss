#quantifying change in phylogenetic diversity from 2018-2023 in turf plots

library(tidyverse)
library(dplyr)
library(picante)
library(geiger)
library(ape)
library(vegan)
library(forcats)
library(broom)
library(janitor)
library(patchwork)
library(car)

#Need to make tree from "Data_cleaning_phylogeny.R" before running anything here

#PD---------------------------------------------------
#calculate PD
pd_allplots <- ses.pd(matrix_forphylogeny, pruned.tree, null.model = c("sample.pool"),
       runs = 5000, include.root=TRUE)

#delete unnecessary columns 
pd_df = subset(pd_allplots, select = -c(ntaxa, pd.obs, pd.rand.mean, 
                                              pd.obs.rank,runs))

#remove space from row names
#row.names(pd_df) <- gsub(' ', '', row.names(pd_df))

#split plots and treatments into separate columns
pd_df$ID <- row.names(pd_df)

#delete 'all' row
pd_df <- pd_df[-c(378),]

#split columns
pd_df <- pd_df %>%
  separate(col = ID, into = c("turfID","tx_site", "year", "plotID"), sep = " _ ")

#save as csv
write.csv(pd_df, file = "Data/PD_byPlot.csv")

#bring in data
pd_df <- read.csv("Data/PD_byPlot.csv")

#figure
pd_fig <- ggplot(data = pd_df, aes(x=year, y=pd.obs.z, group = year))+
  geom_boxplot()+
  facet_wrap(.~ tx_site)+
  theme_bw()+
  geom_hline(yintercept = 0)

plot(pd_fig)

#MPD---------------------------------------------------------
MPD_allplots <- ses.mpd(matrix_forphylogeny, cophenetic(pruned.tree), 
                        null.model = c("sample.pool"), 
                        abundance.weighted = FALSE, runs = 5000, 
                        iterations = 5000)  

#delete unnecessary columns 
MPD_df = subset(MPD_allplots, select = -c(ntaxa, mpd.obs,mpd.rand.mean, mpd.rand.sd,
                                          mpd.obs.rank,runs))
#delete 'all' row and rows that didn't run 
MPD_df <- MPD_df[-c(377),]
MPD_df <- na.omit(MPD_df)

#split plots and treatments into separate columns
MPD_df$ID <- row.names(MPD_df)
MPD_df <- MPD_df %>%
  separate(col = ID, into = c("turfID","tx_site", "year", "plotID"), sep = " _ ")

#save as csv
write.csv(MPD_df, file = "Data/MPD_byPlot.csv")

#figure
mpd_fig <- ggplot(data = MPD_df, aes(x=year, y=mpd.obs.z))+
  geom_boxplot()+
  facet_wrap(.~ tx_site)+
  theme_bw()+
  geom_hline(yintercept = 0)

plot(mpd_fig)

#MNTD---------------------------------------------------------------------------
mntd_allplots <- ses.mntd(matrix_forphylogeny, cophenetic(pruned.tree), 
                        null.model = c("sample.pool"), 
                        abundance.weighted = FALSE, runs = 5000, 
                        iterations = 5000)  

#delete unnecessary columns 
mntd_allplots = subset(mntd_allplots, select = -c(ntaxa, mntd.obs, 
                                                mntd.rand.mean, mntd.rand.sd,
                                                mntd.obs.rank,runs))


#split plots and treatments into separate columns
mntd_allplots$ID <- row.names(mntd_allplots)
mntd_df <- mntd_allplots %>%
  separate(col = ID, into = c("turfID","tx_site", "year", "plotID"), sep = " _ ")

#delete 'all' row and plots that didn't run
mntd_df <- mntd_df[-c(378),]
mntd_df <- na.omit(mntd_df)

#save as csv
write.csv(mntd_df, file = "Data/MNTD_byPlot.csv")

#figure
mntd_fig <- ggplot(data = mntd_df, aes(x=year, y=mntd.obs.z, group = year))+
  geom_boxplot()+
  facet_wrap(.~ tx_site)+
  theme_bw()+
  geom_hline(yintercept = 0)

plot(mntd_fig)


