install.packages("maps")
install.packages("mapdata")

library(maps)
library(mapdata)
library(tidyverse)

ggplot() +
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill=NA, color="black")) +
  coord_quickmap() 
