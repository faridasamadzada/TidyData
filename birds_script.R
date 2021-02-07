library(tidyverse)
library(ggplot2)
library(png)
library(RCurl)
library(cowplot)
library(magick)
library(downloader)

download("https://www.inaturalist.org/attachments/flow_task_outputs/2691992/observations-130313.csv.zip?1612668597", dest="birds.csv.zip", mode="wb") 
unzip("birds.csv.zip", "observations-130313.csv")

birds_ylw <- read.csv("observations-130313.csv") %>% 
  filter(quality_grade == "research") %>% 
  filter(latitude < 49.90451 & latitude > 49.842825) %>% 
  filter(longitude > -119.505413 & longitude < -119.449430)
#downtown defined North End, Central City, and South Pandosy KLO
bird_obs <- birds_ylw %>% 
  group_by(taxon_family_name) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  top_n(10)
#small png
#downloaded from phylopic.org
#Creative common license: https://creativecommons.org/licenses/by/3.0/ 

anatidaepng <- readPNG(getURLContent("http://phylopic.org/assets/images/submissions/23c63b97-e0ab-4b43-89a4-e2358f4f09ec.128.png")) #by Rebecca Groom 
icteridaepng <- readPNG(getURLContent("http://phylopic.org/assets/images/submissions/f783a5c3-8f88-442d-9005-42791b943d7a.128.png")) #by Matt Wilkins
laridaepng <- readPNG(getURLContent("http://phylopic.org/assets/images/submissions/966db6c3-7719-400d-af61-a2d671b293b8.128.png")) #by Rebecca Groom 
passerellidaepng <- readPNG(getURLContent("http://phylopic.org/assets/images/submissions/dd23dc22-77a2-46bb-a64c-d60a0558efb8.128.png")) #by Ferran Sayol
podicipedidaepng <- readPNG(getURLContent("http://phylopic.org/assets/images/submissions/59be555f-7a96-4608-ab71-35ab4f5e77e1.128.png")) #by by Doug Backlund (photo), John E. McCormack, Michael G. Harvey, Brant C. Faircloth, Nicholas G. Crawford, Travis C. Glenn, Robb T. Brumfield & T. Michael Keesey
accipitridaepng <- readPNG(getURLContent("http://phylopic.org/assets/images/submissions/a6ef5684-5683-4a46-aa2d-d39c0d134ba7.128.png")) #by Robert Cooke
ardeidaepng <- readPNG(getURLContent("http://phylopic.org/assets/images/submissions/fb4a4bb7-a50f-4d84-9365-5b25638468e6.128.png")) #by Ferran Sayol 
pandionidaepng <- readPNG(getURLContent("http://phylopic.org/assets/images/submissions/8353477c-dc5d-44b6-ab3c-9d0cdf4d64f7.128.png")) #by Steven Traver
picidaepng <- readPNG(getURLContent("http://phylopic.org/assets/images/submissions/ddd5783c-ded5-48f2-a07d-cc37c83b227b.128.png"))  #by Steven Traver

fringillidaepng <- readPNG(getURLContent("http://phylopic.org/assets/images/submissions/c64e59f8-5d99-4cf5-bcfd-c73022e288e2.128.png"))
#by Francesco Veronesi (vectorized by T. Michael Keesey)


birdplot <- ggplot(bird_obs, aes(x = n, y = reorder(taxon_family_name, n))) + 
  geom_bar(stat = "identity", fill = "black") +
  theme_classic() + 
  scale_x_continuous(limits = c(0, 230), breaks = seq(0, 230, by = 20)) + 
  theme(axis.text.y = element_text(color = "black")) + 
  theme(axis.text.x = element_text(color = "black")) +
  ylab("Family") + 
  xlab("Observations") + ggtitle("Birds in Downtown Kelowna, BC") + labs(caption = "Top 10 families based on iNaturalist observations downloaded using iNaturalist export tool on Feb 6, 2021") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0))) +
  theme(plot.caption = element_text(size=8)) 

bird_plot_images <- ggdraw(birdplot) + 
  draw_image(anatidaepng, x = 0.995, y = 0.90, hjust = 1, vjust = 1, width = 0.1, height = 0.06) +
  draw_image(icteridaepng, x = 0.44, y = 0.82, hjust = 1, vjust = 1, width = 0.11, height = 0.05) + 
  draw_image(laridaepng, x = 0.425, y = 0.75, hjust = 1, vjust = 1, width = 0.09, height = 0.05) +
  draw_image(passerellidaepng, x = 0.385, y = 0.67, hjust = 1, vjust = 1, width = 0.09, height = 0.04) + 
  draw_image(podicipedidaepng, x = 0.375, y = 0.60, hjust = 1, vjust = 1, width = 0.09, height = 0.04) + 
  draw_image(accipitridaepng, x = 0.385, y = 0.54, hjust = 1, vjust = 1, width = 0.099, height = 0.06) +
  draw_image(ardeidaepng, x = 0.37, y = 0.465, hjust = 1, vjust = 1, width = 0.1, height = 0.06) + 
  draw_image(pandionidaepng, x = 0.362, y = 0.40, hjust = 1, vjust = 1, width = 0.1, height = 0.06) +
  draw_image(picidaepng, x = 0.356, y = 0.31, hjust = 1, vjust = 1, width = 0.1, height = 0.06) + 
  draw_image(fringillidaepng, x = 0.35, y = 0.24, hjust = 1, vjust = 1, width = 0.1, height = 0.05)

save_plot("birds_plot.jpg", bird_plot_images) 
