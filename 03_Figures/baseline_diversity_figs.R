library(tidyverse)

#2017 without replication -------------
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

#W/in site transplant richness over years ------------------

#bring in data
richness_baseline_df <- read.csv("Data/baseline_richness_data.csv")

#split ID column to plot
bl_rich_df <- richness_baseline_df %>%
  separate_wider_delim(X,
                       delim = " _ ",
                       names = c("ID", "originSite",
                                 "destinationSite","treatment","year"), 
                       too_many = "merge")

#combine 2017 with other years ------
all_baseline_df <- bind_rows(richness_df_2017, bl_rich_df)

#make figure -------------
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
