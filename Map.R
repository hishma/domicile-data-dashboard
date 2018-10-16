library(maps)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(mapdata)
library(RgoogleMaps)





UnitMap <- read_csv("/Users/bari/R_files/Domicile/DomProject/DomData/Data/UnitMap.csv")
color <- UnitMap$name

Seattle <- GetMap("Seattle, Wa", zoom = 13)

plotmap(lat = UnitMap$lat, lon = UnitMap$lon, API = "google", zoom = 13, col = "purple")


ggplot() + 
  geom_point(data = UnitMap, aes(x=lon, y = lat), color = "name"))