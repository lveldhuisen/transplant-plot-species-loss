library(tidyverse)
library(ggeffects)
library(sjPlot)
library(dplyr)
library(patchwork)

#plot abundance changes in congener pairs to show different behavior

#bring in data
abundance_df1 <- read.csv("Data/abundance_clean2018-2023.csv")
abundance_df1$year <- as.factor(abundance_df1$year)

#get rid of extra control treatments
control.outs <- c("netted_untouched","untouched")
abundance_df1 <- abundance_df %>% filter(!is.na(treatment),
                                    !treatment %in% control.outs)

#Agoseris------------

#filter for only agoseris
agoseris_list <- c("Agoseris_glauca","Agoseris_aurantiaca")
agoseris <- abundance_df1 %>% filter(species %in% agoseris_list)

##year on x axis####
ggplot(agoseris)+
  geom_point(mapping = aes(x = year, y = occurrenceCount, color = treatment)) +
  theme_bw() +
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  facet_wrap(.~species)

##treatment on x axis####
ggplot(agoseris)+
  geom_boxplot(mapping = aes(x = treatment, y = occurrenceCount, color = species)) +
  theme_bw() +
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  scale_x_discrete(labels = c("-2","-1","0","+1","+2"))+
  labs(x = "Treatment", y = "Abundance", color = "Species")+
  ggtitle("Agoseris")

#Draba----------

#Elymus----------

#filter for only elymus
elymus_list <- c("Elymus_repens","Elymus_bakeri")
elymus <- abundance_df1 %>% filter(species %in% elymus_list)

##year on x axis####


##treatment on x axis####
ggplot(elymus)+
  geom_boxplot(mapping = aes(x = treatment, y = occurrenceCount, color = species)) +
  theme_bw() +
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  scale_x_discrete(labels = c("-2","-1","0","+1","+2"))+
  labs(x = "Treatment", y = "Abundance", color = "Species")+
  ggtitle("Elymus")

#Erigeron------------

#filter for only erigeron
erigeron_list <- c("Erigeron_coulteri","Erigeron_elatior","Erigeron_speciosus")
erigeron <- abundance_df1 %>% filter(species %in% erigeron_list)

##treatment on x axis####
ggplot(erigeron)+
  geom_boxplot(mapping = aes(x = treatment, y = occurrenceCount, color = species)) +
  theme_bw() +
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  scale_x_discrete(labels = c("-2","-1","0","+1","+2"))+
  labs(x = "Treatment", y = "Abundance", color = "Species")+
  ggtitle("Erigeron")

#Festuca-----------
#filter for only festuca
festuca_list <- c("Festuca_rubra","Festuca_thurberi")
festuca <- abundance_df1 %>% filter(species %in% festuca_list)

##treatment on x axis####
ggplot(festuca)+
  geom_boxplot(mapping = aes(x = treatment, y = occurrenceCount, color = species)) +
  theme_bw() +
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  scale_x_discrete(labels = c("-2","-1","0","+1","+2"))+
  labs(x = "Treatment", y = "Abundance", color = "Species")+
  ggtitle("Festuca")

#Galium----------------


#Poa----------------


#Senecio-------
#filter for only senecio
senecio_list <- c("Senecio_crassulus","Senecio_integerrimus")
senecio <- abundance_df1 %>% filter(species %in% senecio_list)

##treatment on x axis####
ggplot(senecio)+
  geom_boxplot(mapping = aes(x = treatment, y = occurrenceCount, color = species)) +
  theme_bw() +
  scale_color_viridis_d(begin = 0.2, end = 0.8)+
  scale_x_discrete(labels = c("-2","-1","0","+1","+2"))+
  labs(x = "Treatment", y = "Abundance", color = "Species")+
  ggtitle("Senecio")


#Valeriana---------