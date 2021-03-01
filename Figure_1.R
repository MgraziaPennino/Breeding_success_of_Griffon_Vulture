
######################
#      Figure 1
######################


######################
# Charge libraries
######################

library(rgdal)
library(broom)
library(ggsn)# to add a north arrow and a scale bar to the map
library(rgeos)
library(dplyr)
library(raster)
library(ggplot2)
library(maptools)# note that you don't need to call maptools to run the code below but it needs to be installed.
library(marmap)
library(mapdata)
library(mapproj)
library(cowplot)
library("ggspatial")
library(RColorBrewer)
library("rnaturalearth")
library("rnaturalearthdata")


# set factors to false
options(stringsAsFactors = FALSE)

### Read data
data_gf <- read.csv2("Data_matrix.csv")
str(data_gf)

### Country MAP
world <- ne_countries(scale = "medium", returnclass = "sf")

ply = data.frame(
  lon = c(8,8,8, 10, 10, 10),
  lat = c(38.5,  41, 41.5, 41.5, 41, 38.5))# build a polygon of study area

minimap <- ggplot(data = world) +
  geom_sf(fill="grey", colour="darkgrey") + 
  coord_sf(xlim = c(-9, 17), ylim = c(36, 51), expand = FALSE, datum=NA) +
  geom_polygon(data = ply, aes(x = lon, y = lat), color = "black", 
               fill="#fe9152", alpha = 0.2, size=0.1) +
  xlab(" ") + ylab(" ") + theme_bw()+  theme(panel.spacing = unit(0.1, "cm"))

minimap

### Main MAP
italy <- getData('GADM',country="ITA",level=0)

# convert spatial object to a ggplot ready data frame
sjer_roads_df <- tidy(italy, group  = "id")
# make sure the shapefile attribute table has an id column
italy$id <- rownames(italy@data)
# join the attribute table from the spatial object to the new data frame
sjer_roads_df <- left_join(sjer_roads_df,
                           italy@data,
                           by = "id")

# set map limits
lons = c(8, 12)
lats = c(38.5, 41.5)

# create the breaks- and label vectors
ewbrks <- seq(8,12)
nsbrks <- seq(38.5, 41.5)
ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste(-x, "W"), ifelse(x > 0, paste(x, "E"),x))))
nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste(x, "S"), ifelse(x > 0, paste(x, "N"),x))))



main <- ggplot()+

  # add coastline
  geom_polygon(data = sjer_roads_df , aes(x = long, y = lat, group = group), 
               fill= "grey", color = "darkgrey") + 
  
  
  # add points
  geom_point(data = data_gf, aes(x = Longitude , y =Latitude),
             colour = "black", fill = "black", 
             stroke = .5, size = 0.8, 
             alpha = 1, shape = 21) +
  


  # configure projection and plot domain
  coord_map(xlim = lons, ylim = lats) +
  # formatting
  scale_x_continuous(breaks = ewbrks, labels = ewlbls, expand = c(0, 0)) +
  scale_y_continuous(breaks = nsbrks, labels = nslbls, expand = c(0, 0)) +
  ylab(" ")+xlab(" ")+
  theme_bw() + theme(axis.text.x= element_text(size=8), axis.text.y= element_text(size=8))

main

# COMPLETE MAIN + MINIMAP

areaest<-ggdraw() +
  draw_plot(main) +
  draw_plot(minimap, x =0.55, y = 0.58, width = 0.35, height = 0.45) # posicion minimapa

areaest
ggsave("Figure_1.jpg")


library("ggspatial")
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(8, 12), ylim = c(38.5, 41.5))+ theme_classic()
ggsave("scale.jpg")
