library(RCurl)
library(tidyverse)
library(zoo)
library(gganimate)
library(gifski)
library(av)

#downloaded on Feb 1, 2021 
global_mobility_report <- 
read.csv(url("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"))

yyzmob <- global_mobility_report %>% 
  filter(country_region == "Canada") %>% 
  filter(sub_region_1 == "Ontario") %>% 
  filter(sub_region_2 == "Toronto Division")
yyzmob$date <- as.Date(yyzmob$date, format="%Y-%m-%d")



yyzmob_g <- yyzmob %>% 
  plyr::rename(c("retail_and_recreation_percent_change_from_baseline" = "Recreation", "grocery_and_pharmacy_percent_change_from_baseline" = "Grocery", "parks_percent_change_from_baseline" = "Parks", "transit_stations_percent_change_from_baseline" = "Transit", "workplaces_percent_change_from_baseline" = "Workplaces", "residential_percent_change_from_baseline" = "Residential")) %>% 
  gather("Activity", "change_from_baseline", Residential, Workplaces, Transit, Parks, Grocery, Recreation)

daily_change_Toronto <- ggplot(yyzmob_g, aes(x = date, y = change_from_baseline)) + geom_line() + facet_wrap(~Activity) +
  geom_hline(yintercept = 0, color = "#e82063", linetype = "dashed",lwd=0.8) +
  scale_x_date(date_breaks = "3 month", date_labels =  "%b\n%Y") + 
  theme_classic()  +
  scale_y_continuous(breaks = seq(-100, 200, by = 50)) + 
  ylab("Percent change relative to baseline") +
  xlab("") +
  ggtitle("Toronto Mobility Data") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Data source: Google Covid-19 Community Mobility Reports")


daily_change_Toronto + transition_reveal(date)
anim_save("google_mobility_Toronto.gif", animation = last_animation())