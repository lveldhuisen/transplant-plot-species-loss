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

#import & prune S&B phylogeny-------------------------------------------------
setwd("~/Library/CloudStorage/OneDrive-UniversityofArizona/Arizona PhD/Research/RMBL phylogeny/Smith&Brown18") #phylogeny not stored in R project

##import S&B18 tree and check data## 
SBtree <- read.tree(file = "ALLMB.tre")
write.tree(SBtree)
is.rooted(SBtree)

##prune tree to include all species from all treatments and control groups, including 2017##
#bring in raw data
abundance_df <- read.csv("occurance2017-2023.csv")

#remove non-species from species column
gc.outs <- c("litter", "bare_soil", "rock")

#filter data for things you never want
abundance_forphylogeny <- abundance_df %>% filter(!is.na(treatment),
                                         !species %in% gc.outs)

#reformat data to be community data matrix
matrix_forphylogeny <- pivot_wider(abundance_forphylogeny, names_from = species, 
                                    values_from = occurrenceCount)

#substitute species names that aren't in phylogeny (see replacement list in Google Drive)
matrix_forphylogeny <- matrix_forphylogeny %>% 
  rename(
    Agoseris_heterophylla = Agoseris_sp.,
    Agoseris_glauca_var._dasycephala = Agoseris_glauca,
    Aquilegia_coerulea = Aquilegia_caerulea,
    Orobanche_fasciculata = Aphyllon_fasciculatum,
    Carex_nelsonii = Carex_sp.,
    Chamerion_angustifolium = Chamaenerion_angustifolium,
    Epilobium_ciliatum = Epilobium_sp.,
    Erigeron_pinnatisectus = Erigeron_coulteri,
    Erigeron_grandiflorus = Erigeron_elatior,
    Erigeron_compositus = Erigeron_sp., 
    Festuca_rubra_subsp._rubra = Festuca_rubra,
    Helianthella_uniflora = Helianthella_quinquenervis,
    Heterotheca_villosa = Heterotheca_pumila,
    Hydrophyllum_capitatum_var._capitatum = Hydrophyllum_capitatum,
    Lupinus_argenteus = Lupinus_sp.,
    Poa_pratensis_subsp._pratensis = Poa_pratensis,
    Polygonum_douglasii_subsp._douglasii = Polygonum_douglasii,
    Rhodiola_integrifolia = Sedum_integrifolium,
    Senecio_triangularis = Senecio_integerrimus,
    Senecio_serra = Senecio_sp., 
    Achnatherum_nelsonii = Stipa_nelsonii, 
    Symphyotrichum_foliaceum = Symphyotrichum_ascendens, 
    Bouteloua_gracilis = unknown_grass,
    Veratrum_virginicum = Veratrum_californicum
  )

#make column for row IDs with turf id, tx & year
matrix_forphylogeny$ID = NA
matrix_forphylogeny$ID <- paste(matrix_forphylogeny$treatmentOriginGroup, "_",
                                matrix_forphylogeny$year, "_",
                        matrix_forphylogeny$originPlotID)
matrix_forphylogeny <- matrix_forphylogeny %>% relocate(ID)

#remove extra columns
matrix_forphylogeny = subset(matrix_forphylogeny, select = -c(
  turfID, originSite, destinationSite,originPlotID,date_yyyymmdd,
  treatment,treatmentOriginGroup, functionalGroup, unknownMorpho, year, percentCover, X, X.1) )

#replace NAs with 0s
matrix_forphylogeny[is.na(matrix_forphylogeny)] <- 0

#switch treatments to row names for the matrix 
matrix_forphylogeny <- matrix_forphylogeny %>%
  group_by(ID) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )
#make row names
matrix_forphylogeny <- matrix_forphylogeny %>% column_to_rownames(var = "ID")

#sum columns to have column with all species for phylogeny
species_sums <- colSums(matrix_forphylogeny)

# Convert column sums to a data frame row
sum_row <- as.data.frame(t(species_sums))
names(sum_row) <- names(matrix_forphylogeny)  # Ensure column names match

rownames(sum_row) <- c("all") 

#add sum df to community matrix
matrix_forphylogeny <- rbind(matrix_forphylogeny, sum_row)

#get rid of unknowns 
matrix_forphylogeny = subset(matrix_forphylogeny, select = -c(moss, unknown_forb,
                                                              Unknown_round_leaves,
                                                              unknown_seedling,
                                                              Senecio_crassulus))

#prune tree
pruned.tree <- treedata(SBtree, unlist(matrix_forphylogeny[452,matrix_forphylogeny[452,]>0]), warnings = F)$phy
plot(pruned.tree)

#PD for treatments and years---------------------------------------------------
#calculate PD
pd_allplots <- ses.pd(matrix_forphylogeny, pruned.tree, null.model = c("sample.pool"),
       runs = 5000, include.root=TRUE)

#delete unnecessary columns 
pd_allplots <- pd_allplots = subset(pd_allplots, select = -c(ntaxa, pd.obs, 
                                                             pd.rand.mean, 
                                                             pd.obs.rank,runs))


#split plots and treatments into separate columns
pd_allplots$ID <- row.names(pd_allplots)
pd_df <- pd_allplots %>%
  separate(col = ID, into = c("tx_site", "year", "plotID"), sep = " _ ")

#delete 'all' row
pd_df <- pd_df[-c(452),]

#figure
pd_fig <- ggplot(data = pd_df, aes(x=year, y=pd.obs.z))+
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
MPD_allplots = subset(MPD_allplots, select = -c(ntaxa, mpd.obs, 
                                                             mpd.rand.mean, mpd.rand.sd,
                                                             mpd.obs.rank,runs))


#split plots and treatments into separate columns
MPD_allplots$ID <- row.names(MPD_allplots)
MPD_df <- MPD_allplots %>%
  separate(col = ID, into = c("tx_site", "year", "plotID"), sep = " _ ")

#delete 'all' row
MPD_df <- MPD_df[-c(452),]

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
  separate(col = ID, into = c("tx_site", "year", "plotID"), sep = " _ ")

#delete 'all' row
mntd_df <- mntd_df[-c(452),]

#figure
mntd_fig <- ggplot(data = mntd_df, aes(x=year, y=mntd.obs.z))+
  geom_boxplot()+
  facet_wrap(.~ tx_site)+
  theme_bw()+
  geom_hline(yintercept = 0)

plot(mntd_fig)
