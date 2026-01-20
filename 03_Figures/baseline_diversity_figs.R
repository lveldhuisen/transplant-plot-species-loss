library(tidyverse)

# Richness -----------
## 2017 without replication -------------
richness_df_2017 <- read.csv("Data/2017_richness_data.csv")

#split ID column to plot
richness_df_2017 <- richness_df_2017 %>%
  separate_wider_delim(X,
                       delim = " _ ",
                       names = c("ID", "originSite",
                                 "destinationSite","treatment"))

richness_df_2017$year <- "2017"
richness_df_2017 <- richness_df_2017 %>%
  rename(richness_baseline_df = richness_baseline_2017)

## W/in site transplant richness over years ------------------

#bring in data
richness_baseline_df <- read.csv("Data/baseline_richness_data.csv")

#split ID column to plot
bl_rich_df <- richness_baseline_df %>%
  separate_wider_delim(X,
                       delim = " _ ",
                       names = c("ID", "originSite",
                                 "destinationSite","treatment","year"), 
                       too_many = "merge")

## combine 2017 with other years ------
all_baseline_df <- bind_rows(richness_df_2017, bl_rich_df)

## make figure -------------
bl_richness_fig <- ggplot(all_baseline_df, aes(x=year, y= richness_baseline_df, colour = originSite))+
  geom_boxplot()+
  theme_bw(base_size = 20)+
  xlab("Year")+
  ylab("Richness") +
  labs(colour = "Site") + 
  stat_summary(fun = mean, geom = "line", aes(group = originSite), linewidth = 1)+
  scale_color_manual(values=c("#287C8EFF", "#35B779FF", "#FDE725FF"),
                     labels = c("High","Mid","Low"))

plot(bl_richness_fig)
ggsave("Figures/baseline_richness_overtime.png", dpi = 600, height = 10, width = 15)

# Faith's phylogenetic diversity --------------

## 2017 without replication -----

# convert row names back to column to later split
pd_2017 <- pd_2017 %>% 
  rownames_to_column(var = "group")

#split ID column to plot

pd_2017 <- pd_2017 %>%
  separate_wider_delim(group,
                       delim = " _ ",
                       names = c("ID", "originSite",
                                 "destinationSite","treatment"))

pd_2017$year <- "2017"

## 2018-2023 within site transplants only ------

# convert row names back to column to later split
pd_baseline <- pd_baseline %>% 
  rownames_to_column(var = "group")

#split ID column to plot

pd_baseline <- pd_baseline %>%
  separate_wider_delim(group,
                       delim = " _ ",
                       names = c("ID", "originSite",
                                 "destinationSite","treatment",
                                 "year"))

## combine 2017 with other years ------
all_pd_baseline <- bind_rows(pd_2017, pd_baseline)

## rename origin sites
all_pd_baseline <- all_pd_baseline %>%  
  mutate(originSite = case_match(originSite,
                                 "Upper Montane" ~ "Low elevation",
                                 "Pfeiler" ~ "Mid elevation",
                                 "Monument" ~ "High elevation",
                                  .default = originSite
  ))

all_pd_baseline <- all_pd_baseline %>%
  mutate(group = fct_relevel(originSite, "Low elevation", "Mid elevation", "High elevation"))

## make PD figure ------

bl_pd_fig <- ggplot(all_pd_baseline, aes(x=year, y= pd.obs.z, colour = originSite))+
  geom_boxplot()+
  theme_bw(base_size = 20)+
  xlab("Year")+
  ylab("SES PD") +
  labs(colour = "Site") + 
  stat_summary(fun = mean, geom = "line", aes(group = originSite), linewidth = 1)+
  scale_color_manual(values = c("Low elevation" = "#FDE725FF", "Mid elevation" = "#35B779FF", "High elevation" = "#287C8EFF"),
                     limits = c("High elevation", "Mid elevation", "Low elevation"))
scale_color_manual(values=c("#287C8EFF", "#35B779FF", "#FDE725FF"))


plot(bl_pd_fig)
ggsave("Figures/baseline_PD_overtime.png", dpi = 600, height = 10, width = 15)


