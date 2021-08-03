library(tidytuesdayR)
library(tidyverse)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load(2021, week = 31)

olympics <- tuesdata$olympics

mean_age_year <- olympics %>% 
  filter(season == "Summer") %>% 
  filter(sport != "Art Competitions") %>% 
  group_by(year, sex) %>% 
  distinct(id, .keep_all = TRUE)  %>% 
  drop_na(age) %>% 
  mutate(mean_age = mean(age))  %>% 
  select(year, mean_age, sex) %>% 
  arrange(year) %>% 
  distinct() 



mean_age_year

olympics_plot <- mean_age_year %>% 
ggplot(aes(x = year, y = mean_age, color = sex)) + 
geom_point(size = 2) + geom_line() + 
theme_minimal() + 
theme(axis.title.x=element_blank()) +
ggtitle("Average Age of Olympic Athletes Through The Years", "With some exceptions, mean age of female athletes is consistently lower than that of male athletes") + 
theme(plot.subtitle=element_text(size=7.5, face = "bold")) + 
labs(caption = "Data Source: Kaggle/TidyTuesday") + 
ylab("Mean Age") + 
theme(legend.position = "none") + 
scale_color_manual(values = c("#650e9c", "#dec11f")) + 
theme(panel.grid = element_blank()) + 
scale_x_continuous(breaks = seq(1890, 2016, by = 10)) +
scale_y_continuous(breaks = seq(10, 60, by = 5)) +
annotate("text", x = 1937, y = 53.5, label = 
           "          In 1904, at the age of 63, 
           American Archer Lida Peyton 'Eliza' Pollock
         became the oldest woman to 
         win an Olympic Gold Medal
         ", color = "black", size = 3) + 
geom_curve(aes(x = 1924, xend = 1904, y = 59.5, yend = 49.8),
             arrow = arrow(length = unit(1.0, "mm")), color = "black", curvature = 0.4) + 
geom_curve(aes(x = 1926.5, xend = 1927, y = 16, yend = 19.78),
             arrow = arrow(length = unit(1.0, "mm")), color = "black", curvature = -0.4) + 
annotate("text", x = 1960, y = 13.5, label = 
           "          In 1928, at the age of 11, Italian Gymnast Luigina Giavotti
         became the youngest woman to win an Olympic Medal
         ", color = "black", size = 3) +
annotate("text", x = 2014, y = 30, label = "Men", color = "#dec11f", size = 4) + 
annotate("text", x = 2014, y = 23, label = "Women", color = "#650e9c", size = 4) 


save_plot("olympics_plot.jpg", olympics_plot)
