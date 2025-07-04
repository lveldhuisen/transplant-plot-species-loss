library(tidyverse) 
library(lme4) #modelling
library(lmerTest) #provides p values
library(performance) #model diagnostics
library(ggeffects) #easy prediction & comparison
library(sjPlot) #visualize random effects
library(vegan) #community diversity
library(stringr) #to remove spaces
library(dplyr) #data cleaning

#Shannon diversity calculations------------------------------------------------

##Shannon for three different controls only, no transplant data#######
#*****has not been updated for percentCover, occurrenceCount is wrong***##

#calculate shannon diversity
comm_matrix_controls <- read.csv("Data/Community_matrix_controlsonly.csv")

shannon_controls <- diversity(comm_matrix_controls, index = "shannon")

#make dataframe including plot numbers and reformat to use for plot
shannon_df_controls <- as.data.frame(shannon_controls)
shannon_df_controls$ID <- row.names(shannon_df_controls)

shannon_df_controls <- shannon_df_controls %>%
  separate(col = ID, into = c("tx_site", "year", "plotID","treatment"), sep = " _ ")

###figure for control plots only#######
shannon_fig_controls <- ggplot(data = shannon_df_controls, aes(x=year, y=shannon_controls, color = treatment))+
  geom_boxplot()+
  #facet_wrap(.~ treatment)+
  theme_bw()

plot(shannon_fig_controls)

##Shannon for each individual plot#####
comm_matrixID <- read.csv("Data/Community_matrix_ByPlot.csv")

#change plots to row names
comm_matrixID <- comm_matrixID %>% column_to_rownames(var = "X")

#calculate shannon diversity
shannon_plots <- diversity(comm_matrixID, index = "shannon")

#make dataframe including plot numbers and reformat to use for plot
shannon_df_plotID <- as.data.frame(shannon_plots)
shannon_df_plotID$ID <- row.names(shannon_df_plotID)

shannon_df_plotID <- shannon_df_plotID %>%
  separate(col = ID, into = c("turfID","originSite","destinationSite",
                              "treatment","year"), sep = " _ ")

###figure including variation between plots of same tx#######
shannon_fig_plots <- ggplot(data = shannon_df_plotID, aes(x=year, y=shannon_plots))+
  geom_boxplot()+
  theme_bw()+
  facet_wrap(.~ site_tx)
plot(shannon_fig_plots)

write.csv(shannon_df_plotID, file = "Data/Shannon_ByPlot2018-2023.csv")


#calculate simpson's diversity--------------------------------------------------
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

#species richness-----------------
richness_df <- specnumber(comm_matrixID, groups = comm_matrixID$X, MARGIN = 1)
richness_df <- as.data.frame(richness_df)    
write.csv(richness_df, "Data/richness_data.csv")

# baseline diversity for each origin site before transplant --------
## richness ---------
base_richness <- abundance_df_baseline %>%
  group_by(turfID) %>%
  summarise(UniqueCount = n_distinct(species))


