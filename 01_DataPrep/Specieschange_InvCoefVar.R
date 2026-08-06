library(tidyverse)

#run all code in "Data_prep_species_abundancechance.R" before running this

#generate ICV values------------

#Upper Montane####

##within site transplant#######
# Extract coefficients and calculate ICV
# Total across plots
um_win_reg <- um_win_reg %>% 
  group_by(species, year) %>% 
  summarise_if(
    is.numeric,
    sum,
    na.rm = TRUE
  )

um_win_reg$year <- as.numeric(um_win_reg$year) # Make year numeric

# Do regression 
um_win_model_icv <- dlply(um_win_reg, "species", function(um_win_reg) {
  lm(percentCover ~ year, data = um_win_reg)
})

# Extract coefficients and calculate ICV
um_win_values_icv <- ldply(um_win_model_icv, function(model) {
  coefs <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  slope <- coefs["year"]
  slope_se <- se["year"]
  
  # Calculate inverse coefficient of variation
  icv <- slope / slope_se
  
  data.frame(
    intercept = coefs["(Intercept)"],
    slope = slope,
    slope_se = slope_se,
    icv = icv
  )
})

#make plot for visual
ggplot(um_win_reg,
       aes(x = year, y = percentCover)) +
  geom_point() +
  # add regression lines
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~species)+
  stat_poly_eq(use_label(c("eq")))

##cooled 1#######
um_c1_values_icv <- ldply(um_c1_model, function(model) {
  coefs <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  slope <- coefs["year"]
  slope_se <- se["year"]
  
  # Calculate inverse coefficient of variation
  icv <- slope / slope_se
  
  data.frame(
    intercept = coefs["(Intercept)"],
    slope = slope,
    slope_se = slope_se,
    icv = icv
  )
})

##cooled 2######
um_c2_values_icv <- ldply(um_c2_model, function(model) {
  coefs <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  slope <- coefs["year"]
  slope_se <- se["year"]
  
  # Calculate inverse coefficient of variation
  icv <- slope / slope_se
  
  data.frame(
    intercept = coefs["(Intercept)"],
    slope = slope,
    slope_se = slope_se,
    icv = icv
  )
})

#Pfeiler######

##within site transplant#######
pf_win_values_icv <- ldply(pf_win_model, function(model) {
  coefs <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  slope <- coefs["year"]
  slope_se <- se["year"]
  
  # Calculate inverse coefficient of variation
  icv <- slope / slope_se
  
  data.frame(
    intercept = coefs["(Intercept)"],
    slope = slope,
    slope_se = slope_se,
    icv = icv
  )
})

##cooled 1####
pf_c1_values_icv <- ldply(pf_c1_model, function(model) {
  coefs <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  slope <- coefs["year"]
  slope_se <- se["year"]
  
  # Calculate inverse coefficient of variation
  icv <- slope / slope_se
  
  data.frame(
    intercept = coefs["(Intercept)"],
    slope = slope,
    slope_se = slope_se,
    icv = icv
  )
})

##warmed 1####
pf_w1_values_icv <- ldply(pf_w1_model, function(model) {
  coefs <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  slope <- coefs["year"]
  slope_se <- se["year"]
  
  # Calculate inverse coefficient of variation
  icv <- slope / slope_se
  
  data.frame(
    intercept = coefs["(Intercept)"],
    slope = slope,
    slope_se = slope_se,
    icv = icv
  )
})

#Monument######

##within site transplant#######
mo_win_values_icv <- ldply(mo_win_model, function(model) {
  coefs <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  slope <- coefs["year"]
  slope_se <- se["year"]
  
  # Calculate inverse coefficient of variation
  icv <- slope / slope_se
  
  data.frame(
    intercept = coefs["(Intercept)"],
    slope = slope,
    slope_se = slope_se,
    icv = icv
  )
})

##warmed 1####
mo_w1_values_icv <- ldply(mo_w1_model, function(model) {
  coefs <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  slope <- coefs["year"]
  slope_se <- se["year"]
  
  # Calculate inverse coefficient of variation
  icv <- slope / slope_se
  
  data.frame(
    intercept = coefs["(Intercept)"],
    slope = slope,
    slope_se = slope_se,
    icv = icv
  )
})

##warmed 2####
mo_w2_values_icv <- ldply(mo_w2_model, function(model) {
  coefs <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  slope <- coefs["year"]
  slope_se <- se["year"]
  
  # Calculate inverse coefficient of variation
  icv <- slope / slope_se
  
  data.frame(
    intercept = coefs["(Intercept)"],
    slope = slope,
    slope_se = slope_se,
    icv = icv
  )
})

#Format each table to include treatment, origin site, remove NAs-----------

##Upper Montane####
###within site####

#get rid of intercept column
um_win_values_icv = subset(um_win_values_icv, select = -c(intercept))

#add columns for treatment and origin site
um_win_values_icv$originSite = "Upper Montane"
um_win_values_icv$treatment = "within_site_transplant"

#remove NAs
um_win_values_icv <- na.omit(um_win_values_icv)

#save as csv
write_csv(um_win_values_icv, "Data/Species_change/UM_win_slopesICV.csv")

###cooled 1####

#get rid of intercept column
um_c1_values_icv = subset(um_c1_values_icv, select = -c(intercept))

#add columns for treatment and origin site
um_c1_values_icv$originSite = "Upper Montane"
um_c1_values_icv$treatment = "cooled_one"

#remove NAs
um_c1_values_icv <- na.omit(um_c1_values_icv)

#save as csv
write_csv(um_c1_values_icv, "Data/Species_change/UM_c1_slopes_icv.csv")

###cooled 2#####

#get rid of intercept column
um_c2_values_icv = subset(um_c2_values_icv, select = -c(intercept))

#add columns for treatment and origin site
um_c2_values_icv$originSite = "Upper Montane"
um_c2_values_icv$treatment = "cooled_two"

#remove NAs
um_c2_values_icv <- na.omit(um_c2_values_icv)

#save as csv
write_csv(um_c2_values_icv, "Data/Species_change/UM_c2_slopes_icv.csv")

##Pfeiler#####

###within site transplant#####

#get rid of intercept column
pf_win_values_icv = subset(pf_win_values_icv, select = -c(intercept))

#add columns for treatment and origin site
pf_win_values_icv$originSite = "Pfeiler"
pf_win_values_icv$treatment = "within_site_transplant"

#remove NAs
pf_win_values_icv <- na.omit(pf_win_values_icv)

#save as csv
write_csv(pf_win_values_icv, "Data/Species_change/Pfeiler_win_slopes_icv.csv")

###cooled 1#####

#get rid of intercept column
pf_c1_values_icv = subset(pf_c1_values_icv, select = -c(intercept))

#add columns for treatment and origin site
pf_c1_values_icv$originSite = "Pfeiler"
pf_c1_values_icv$treatment = "cooled_one"

#remove NAs
pf_c1_values_icv <- na.omit(pf_c1_values_icv)

#save as csv
write_csv(pf_c1_values_icv, "Data/Species_change/Pfeiler_c1_slopes_icv.csv")

###warmed one####

#get rid of intercept column
pf_w1_values_icv = subset(pf_w1_values_icv, select = -c(intercept))

#add columns for treatment and origin site
pf_w1_values_icv$originSite = "Pfeiler"
pf_w1_values_icv$treatment = "warmed_one"

#remove NAs
pf_w1_values_icv <- na.omit(pf_w1_values_icv)

#save as csv
write_csv(pf_w1_values_icv, "Data/Species_change/Pfeiler_w1_slopes_icv.csv")

##Monument#######

###within site transplant#####

#get rid of intercept column
mo_win_values_icv = subset(mo_win_values_icv, select = -c(intercept))

#add columns for treatment and origin site
mo_win_values_icv$originSite = "Monument"
mo_win_values_icv$treatment = "within_site_transplant"

#remove NAs
mo_win_values_icv <- na.omit(mo_win_values_icv)

#save as csv
write_csv(mo_win_values_icv, "Data/Species_change/Monument_within_slopes_icv.csv")

###warmed one#####

#get rid of intercept column
mo_w1_values_icv = subset(mo_w1_values_icv, select = -c(intercept))

#add columns for treatment and origin site
mo_w1_values_icv$originSite = "Monument"
mo_w1_values_icv$treatment = "warmed_one"

#remove NAs
mo_w1_values_icv <- na.omit(mo_w1_values_icv)

#save as csv
write_csv(mo_w1_values_icv, "Data/Species_change/Monument_w1_slopes_icv.csv")

###warmed two#####

#get rid of intercept column
mo_w2_values_icv = subset(mo_w2_values_icv, select = -c(intercept))

#add columns for treatment and origin site
mo_w2_values_icv$originSite = "Monument"
mo_w2_values_icv$treatment = "warmed_two"

#remove NAs
mo_w2_values_icv <- na.omit(mo_w2_values_icv)

#save as csv
write_csv(mo_w2_values_icv, "Data/Species_change/Monument_w2_slopes_icv.csv")

# make into one dataframe ----------

# Put all your dataframes in a list
df_list <- list(um_win_values_icv, um_c1_values_icv, um_c2_values_icv,
                pf_win_values_icv, pf_c1_values_icv, pf_w1_values_icv,
                mo_win_values_icv, mo_w1_values_icv, mo_w2_values_icv)

# Stack them all at once
combined <- do.call(rbind, df_list)

# update species names

combined <- combined %>% mutate(species = recode(species, 
                                         "Agoseris_glauca" = "Agoseris_glauca_var._dasycephala",
                                         "Aquilegia_caerulea" = "Aquilegia_coerulea",
                                         "Aphyllon_fasciculatum" = "Orobanche_fasciculata",
                                         "Carex_sp." = "Carex_nelsonii",
                                         "Chamaenerion_angustifolium" = "Chamerion_angustifolium",
                                         "Epilobium_sp." = "Epilobium_ciliatum",
                                         "Erigeron_elatior" = "Erigeron_grandiflorus",
                                         "Festuca_rubra" = "Festuca_rubra_subsp._rubra",
                                         "Helianthella_quinquenervis" = "Helianthella_uniflora",
                                         "Heterotheca_pumila" = "Heterotheca_villosa",
                                         "Hydrophyllum_capitatum" = "Hydrophyllum_capitatum_var._capitatum",
                                         "Lupinus_sp." = "Lupinus_argenteus",
                                         "Poa_pratensis" = "Poa_pratensis_subsp._pratensis",
                                         "Polygonum_douglasii" = "Polygonum_douglasii_subsp._douglasii",
                                         "Sedum_integrifolium" = "Rhodiola_integrifolia",
                                         "Senecio_integerrimus" = "Senecio_integerrimus_var.exaltatus",
                                         "Stipa_nelsonii" = "Achnatherum_nelsonii",
                                         "Symphyotrichum_ascendens" = "Symphyotrichum_foliaceum",
                                         "Veratrum_californicum" = "Veratrum_virginicum",
                                      ))
combined$species <- trimws(combined$species)

write_csv(combined, "Data/Species_change/Slopes_ICV_forrevision.csv")

## add AOO and niche breath data into same dataframe ######

# bring in data 
rs_nb_df <- read.csv("Data/Species_change/species_change_forrevision.csv")

rs_nb_df$species <- trimws(rs_nb_df$species)

# rename icv col to match 
names(rs_nb_df)[names(rs_nb_df) == "updated_ICV_rev2026"] <- "icv"

# join with other df
big_df <- left_join(combined, rs_nb_df, by = c("species", "originSite", "treatment"))

# delete extra icv column
big_df = subset(big_df, select = -c(icv.y))

# save csv
write_csv(big_df, "Data/Species_change/all_changes_forrevision.csv")

# add ICV values to new dataframe for correlation with 2017 abundance
ab2017_df <- read.csv("Data/Species_change/2017abundance_slopes.csv")

# join with other df
icv_2017ab <- left_join(ab2017_df, combined, by = c("species", "originSite", "treatment"))

