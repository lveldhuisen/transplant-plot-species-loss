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

##import S&B18 tree and check data#####
SBtree <- read.tree(file = "ALLMB.tre")
write.tree(SBtree)
is.rooted(SBtree)

##prune tree to include all species from all treatments and control groups, including 2017####
#bring in raw data
abundance_df <- read.csv("Data/occurance2017-2023.csv")

#remove non-species from species column
gc.outs <- c("litter", "bare_soil", "rock")

#remove block 6 plots since they were transplanted in 2018
block.outs <- c("mo6-1", "mo6-2", "mo6-3", "mo6-4","mo6-5", "pf6-1",
                "pf6-2", "pf6-3","pf6-4", "um6-1", "um6-2","um6-3","um6-4",
                "um6-5","um6-6")

#filter data for things you never want
abundance_forphylogeny <- abundance_df %>% filter(!is.na(treatment),
                                                  !species %in% gc.outs,
                                                  !originPlotID %in% block.outs)

#reformat data to be community data matrix
matrix_forphylogeny <- pivot_wider(abundance_forphylogeny, names_from = species, 
                                   values_from = occurrenceCount)

#substitute species names that aren't in phylogeny (see replacement list in Google Drive)
matrix_forphylogeny <- matrix_forphylogeny %>% 
  rename(
    Agoseris_glauca_var._dasycephala = Agoseris_sp.,
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
    Achnatherum_nelsonii = Stipa_nelsonii, 
    Symphyotrichum_foliaceum = Symphyotrichum_ascendens,
    Veratrum_virginicum = Veratrum_californicum
  )

#make column for row IDs with turf id, tx & year
matrix_forphylogeny$ID = NA
matrix_forphylogeny$ID <- paste(matrix_forphylogeny$turfID,"_",
  matrix_forphylogeny$treatmentOriginGroup, "_",
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
                                                              Senecio_crassulus,
                                                              unknown_grass))

#prune tree
pruned.tree <- treedata(SBtree, unlist(matrix_forphylogeny[378,matrix_forphylogeny[378,]>0]), warnings = F)$phy
plot(pruned.tree)
