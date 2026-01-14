library(tidyverse)

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


#make figure
bl_richness_fig <- ggplot(bl_rich_df, aes(x=year, y= richness_baseline_df, colour = originSite))+
  geom_boxplot()+
  theme_bw(base_size = 20)+
  xlab("Year")+
  ylab("Richness") +
  labs(colour = "Site") + 
  stat_summary(fun = mean, geom = "line", aes(group = originSite), linewidth = 1)
  scale_color_manual(values=c("#287C8EFF", "#35B779FF", "#FDE725FF"),
                     labels = c("Low","Mid","High"))

plot(bl_richness_fig)
ggsave("Figures/baseline_richness_overtime.png", dpi = 600, height = , width = )