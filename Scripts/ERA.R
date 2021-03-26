rm(list = ls(all= TRUE))

library(rgdal)
library(rgeos)
library(raster)
library(data.table)
library(sp)
library(sf)
library(ncdf4)
library(rnaturalearth)
library(tidyverse)
library(glue)
library(dplyr)
library(circular)
library(scales)
library(viridis)
library(dplyr)
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(plotrix)
library(glue)
library(cowplot)
library(ggthemes)
library(ggplot2)
library(ggnewscale)



map     <- rnaturalearth::ne_coastline(scale = 50, returnclass = "sf")
ext     <- extent(c(103.82, 180, 50.07, 80.56))

####
# Input data ####
####

files <- list.files(path = "Z:/data/bioing/data/Data_Reanalyse/ERA5", pattern = "*.nc", all.files = T, full.names = T)
MODIS <- read.csv2("Z:/data/bioing/data/Projekte/Fires/East Siberia/R_Data/MODIS_east_siberia.txt")
MODIS_raster <- raster("Z:/data/bioing/data/Projekte/Fires/East Siberia/MOD_yakutia1.tif")

fls_Tab <- do.call("rbind", lapply(files, function(x) {
  nf <- nc_open(x)
  tms <- as.POSIXct(nf$var[[1]]$dim[[4]]$vals*60*60, origin = "1900-01-01")
  out <- data.frame(path = x, date = tms, id = 1:length(tms))
  nc_close(nf)
  out
}))


data_coord <- data.frame(location = c("Khamra","Satagay 2.0","Malaya Chabyda","Illirney","Rauchagytgyn", "Elgygytgyn"),
                         lon = c(112.98, 117.99, 129.41,168.20,168.71, 172.90), lat = c(59.99, 63.08,61.92,67.21, 67.80, 67.30))


map <- rnaturalearth::ne_coastline(scale = 50, returnclass = "sf")
ext <- as(extent(c(103.82, 180, 50.07, 80.56)), "SpatialPolygons")

mapCrop <- map %>% 
  st_intersection(st_as_sf(as(ext, "SpatialPolygons")) %>% 
                    st_set_crs(4326)) %>% st_geometry 

MODIS <- select(.data = MODIS,"FID", "LATITUDE","LONGITUDE", "ACQ_DATE", "FRP")
names(MODIS)[names(MODIS) == "ACQ_DATE"] <- "TIME"

MODIS$TIME <- as.POSIXct(x = MODIS$TIME, format = '%d.%m.%Y %H:%M:%S')
MODIS$FRP <- as.numeric(MODIS$FRP)
head(MODIS$TIME)

MODIS_ext <- MODIS %>%
  filter(LONGITUDE >=103.82 & LONGITUDE <= 180 & LATITUDE >= 50.07  & LATITUDE <= 80.56 & FRP >= 1)

for(y in 2019){

cat(glue("\rWir befinden uns im Jahre {y} nach Christus. Ganz Gallien ist nicht mehr von den Römern besetzt."))
subTab <- subset(fls_Tab, as.numeric(format(date, "%Y")) %in% y &
                          as.numeric(format(date, "%m")) %in% c(6:8)) # Monat 6 bis 8 bei mehreren Jahren und Monaten

subFire <- subset(MODIS_ext, as.numeric(format(TIME, "%Y")) %in% y &
                             as.numeric(format(TIME, "%m")) %in% c(6:8))


# Creating a list for every date with wind direction and speed
rasterList <- lapply(unique(subTab$path), function(x) {
  
  u <- raster::crop(brick(x, varname=  "u", level = 1), ext)
  v <- raster::crop(brick(x, varname = "v", level = 1), ext)
  
  dir <- atan2(u, v)*(180/pi)
  dir[] <- ifelse(dir[]<0, 360+dir[], dir[])
  spd <- sqrt((u^2)+(v^2))
  
  list <- list(dir, spd)
})


# Creating a brick for wind direction and speed 
dirBrick <- brick(lapply(rasterList, function(x) x[[1]]))
spdBrick <- brick(lapply(rasterList, function(x) x[[2]]))
medSpd <- calc(spdBrick, median)
medDir <- calc(dirBrick, median)


data_map <- as(medSpd, "SpatialPixelsDataFrame")
data_spd <- as.data.frame(data_map)
colnames(data_spd) <- c("value", "x", "y")

aggrR   <- aggregate(brick(medSpd, medDir), 20)
r_coord <- coordinates(aggrR)
r_coord[,1] <- ifelse(r_coord[,1]>180,NA, r_coord[,1])
arrow <- data.frame(r_coord, geosphere::destPoint(r_coord, aggrR[[2]], aggrR[[1]]*60*60*5))



plotDirSpeedMap <- ggplot() +  
    geom_tile(data = data_spd, aes(x = x, y = y, fill = value), alpha = 0.8) + 
    geom_sf(data = mapCrop)+
    geom_point(mapping = aes(x = lon, y = lat, shape = location), data = data_coord, colour = "black",
               fill = "white", size = 4, stroke = 2)+
    scale_fill_gradientn(colours = rev(viridis::plasma(99)),
                       breaks = round(seq(min(medSpd[]), max(medSpd[]), length = 5), 0))+
    geom_segment(data = subset(arrow, lon>0), aes(x = x, xend = lon, y = y, yend = lat),
                 arrow = arrow(length = unit(0.1, "cm")), colour = "black")+
    scale_shape_manual(name = "Lakes", values = c(0:5))+
    theme_minimal() +
    labs(subtitle = "The average wind direction and speed", fill = "Wind speed\n[m/s]")+
    xlab("") +
    ylab("") +
    ylim(c(50, 80))+
    theme(plot.subtitle = element_text(size = 20, hjust = 0.5, vjust = -3),
          legend.title = element_text(size = 12, vjust = 1),
          legend.text = element_text(size = 8, vjust = 0.75))


####
# Calculating the variance of wind direction and speed ####
####

var_dir <- calc(x = dirBrick, function(x) {
  crc <- circular(x, type = "angles", units = "degrees") # circular erstellen mit Datenpunkten auf Kreis
  out <- quantile(crc, probs = c(0.2,0.8)) #Quantile festlegen
  if(diff(out)<0) { #damit nicht Minuswerte
    diff(out)+360
  } else diff(out)
})

map_var <- as(var_dir, "SpatialPixelsDataFrame")
map_var_data <- as.data.frame(map_var)
colnames(map_var_data) <- c("value", "x", "y")

plotVarMap <- ggplot() +
  geom_tile(data = map_var_data, aes(x = x, y = y, fill = value), alpha = 0.8)+
  geom_sf(data = mapCrop)+
  scale_fill_gradientn(colours = rev(viridis::viridis(99)),
                       breaks = round(seq(min(map_var_data[, 1]), max(map_var_data[, 1]), length = 6), 0))+
  geom_point(mapping = aes(x = LONGITUDE,y = LATITUDE, alpha= FRP, size = FRP),
             data = subFire, colour = "white", show.legend = F) +
  scale_alpha(range = c(1/100,1/1000))+
  geom_point(mapping = aes(x = lon, y = lat, shape = location), data = data_coord,
             size = 4, stroke = 2, show.legend = F)+
  scale_shape_manual(values = c(0:5))+
  theme_minimal() +
  labs(subtitle = "The variance of wind direction", fill = "Interquantile\n[Q0.8-Q0.2]")+
  xlab("") +
  ylab("") +
  ylim(c(50, 80))+
  theme(plot.subtitle = element_text(size = 20, hjust = 0.5, vjust = -3),
        legend.title = element_text(size = 12, vjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0.75))





figure_1 <- ggarrange(plotDirSpeedMap, plotVarMap, nrow = 1, ncol = 2, widths = c(1,1),heights = c(1,1),
                      common.legend = F, legend = "bottom")
        

####
# Plotting windrose ####
####
extr_dir <- raster::extract(dirBrick, data_coord[, 2:3])
extr_spd <- raster::extract(spdBrick, data_coord[, 2:3])
extr_Tab <- data.frame(loc = rep(data_coord$location, ncol(extr_dir)), # für jedes Datum einmal 1:4 durchlaufen lassen
                       time = rep(subTab$date, each = nrow(extr_dir)), 
                       dir = c(extr_dir), spd = c(extr_spd)) 

head(extr_Tab)

plts <- lapply(data_coord$location, function(i) {
  c_sub <- subset(extr_Tab, extr_Tab$loc == i) # bei i wird auf data_coord zugriffen!!! 
  breaks <- seq(0, 360, 10)
  bins <- cut(c_sub$dir, breaks)
  data_bins <- c_sub %>%
    mutate(bins = cut(c_sub$dir, breaks, labels = seq(5, 360, 10)), 
           bins = as.numeric(as.character(bins))) %>%
    group_by(bins) %>%
    summarise(count = n(), spd = median(spd))
    ggplot(data_bins)+
    geom_bar(aes(bins, count , fill = spd), stat = "identity", show.legend = F)+
    scale_fill_gradientn(colours = "grey20", breaks = round(seq(3, 16, length = 4), 0), 
                         limits = c(0,16))+
    coord_polar(start = 0) + theme_minimal()+
    ylab("Count")+
    theme(plot.title = element_text(hjust = 0.5, size = 20, vjust = -2))+
    scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 315, by = 45),
                       labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))+
    theme(axis.text.x = element_text(size = 14, face = 'bold'))
})


figure_2 <- ggarrange(plts[[6]],plts[[5]],plts[[3]],plts[[1]],plts[[2]],plts[[4]],
                      labels = c("Satagay 2.0","Malaya Chabyda","Rauchagytgyn","Khamra","Illirney","Elgygytgyn"), 
                      vjust = 2.25, font.label = list(size = 14, face = "italic",common.legend = TRUE, legend = "bottom"))


png(glue("C:/Users/vreichel/Documents/GitHub/SiberianWindSOMs/WindPlot{y}.png"), width = 1200, height = 1200)
g <- grid.arrange(figure_1, figure_2, nrow = 3,
                  ncol = 3, layout_matrix = rbind(c(1,1), c(2,2)))
print(annotate_figure(g, top = text_grob(glue("Wind direction and speed in East Siberia {y}"),vjust = 0.8, hjust = 0.5, face = "bold", size = 30)))

dev.off()

} ### end

