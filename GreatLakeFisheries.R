library(tidytuesdayR)
library(tidyverse)
library(readxl)
library(streamgraph)
library(htmlwidgets)


tuesdata <- tidytuesdayR::tt_load('2021-06-08')

tuesdata$fishing %>% 
  group_by(species) %>% 
  summarize()
  
burbot <- tuesdata$fishing %>% 
select(year, species, values) %>% 
drop_na() %>% 
filter(species == "Burbot") %>% 
group_by(year) %>% 
summarize("Burbot" = sum(values))

laketrout <- tuesdata$fishing %>% 
  select(year, species, values) %>% 
  drop_na() %>% 
  filter(species == "Lake Trout") %>% 
group_by(year) %>% 
summarize("Lake Trout" = sum(values))

yellowperch <- tuesdata$fishing %>% 
  select(year, species, values) %>% 
  drop_na() %>% 
  filter(species == "Yellow Perch") %>% 
group_by(year) %>% 
summarize("Yellow Perch" = sum(values))

rainbowsmelt <- tuesdata$fishing %>% 
  select(year, species, values) %>% 
  drop_na() %>% 
  filter(species == "Rainbow Smelt") %>% 
group_by(year) %>% 
summarize("Rainbow Smelt" = sum(values))

alewife <- tuesdata$fishing %>% 
  select(year, species, values) %>% 
  drop_na() %>% 
  filter(species == "Alewife") %>% 
group_by(year) %>% 
summarize("Alewife" = sum(values))

whitefish <- tuesdata$fishing %>% 
  select(year, species, values) %>% 
  drop_na() %>% 
  filter(species == "Lake Whitefish") %>% 
group_by(year) %>% 
summarize("Lake Whitefish" = sum(values))

walleye <- tuesdata$fishing %>% 
  select(year, species, values) %>% 
  drop_na() %>% 
  filter(species == "Walleye") %>% 
group_by(year) %>% 
summarize("Walleye" = sum(values))

fishes <- plyr::join_all(list(burbot, yellowperch, alewife, rainbowsmelt, whitefish, laketrout, walleye), type = "full") %>% 
          gather(species, total_values, "Burbot":"Walleye")
          
strmfsh <- streamgraph(fishes, key="species", value="total_values", date="year", height="300px", width="1000px", interactive = TRUE) %>% 
sg_legend(show=TRUE, label="Species: ")  %>%
streamgraph::sg_fill_tableau() %>% 
streamgraph::sg_title("Great Lakes Fish Species Composition 1870-2015") 

saveWidget(strmfsh, file = "./GreatLakesFishes_Widget.html")

