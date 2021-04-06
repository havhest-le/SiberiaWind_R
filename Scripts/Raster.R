rm(list = ls(all= TRUE))


library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(sf)
library(ncdf4)
library(rnaturalearth)
library(tidyverse)
library(data.table)
library(glue)
library(interp)
library(contourPlot)


#####
# Input data
#####

data_tab <- read.csv(file = "N:/bioing/user/slisovsk/crdsTab.csv", header = T, col.names=c("lake","date", "lon", "lat", "buffer"))
head(data_tab)
lakes   <- read_sf("Data/lakesSHP/lakes_sf.shp") 
data_raster <- raster()
map     <- read_sf("Data/ne_50m_land/ne_50m_land.shp") %>% st_geometry()
ext     <- extent(c(95, 200, 50, 80))
proj   <- glue("+proj=laea +lon_0={mean(ext[1:2])} +lat_0={mean(ext[3:4])}")



# plot(data_tab[,c("lon", "lat")], type = "n")
# plot(map, add = T)
# points(data_tab[data_tab$buffer > 10, c("lon", "lat")], pch = 16, cex = 0.4, col= "blue")
# plot(lakes$geometry, add = T)
# plot(lakes$geometry, lwd = 13, add = T, col = "red")


# single lakes
Elgy <- data_tab  %>%
  group_by(lake) %>%
  filter(lake == "Elgy")


Kham <- data_tab  %>%
  group_by(lake) %>%
  filter(lake == "Khamra")

Ill <- data_tab  %>%
  group_by(lake) %>%
  filter(lake == "Ill")



# Ell
raster_data_el <- raster(nrows = 1000000, ncols = 1000000, xmn = min(Elgy$lon), xmx = max(Elgy$lon), ymn = min(Elgy$lat), ymx = max(Elgy$lat), res = .25,
                      crs = 4326)
raster_El <- rasterize(Elgy[,c("lon", "lat")], raster_data_el, fun = 'count')
plot(raster_El)
p_El <- rasterToPoints(raster_El)
data_El <- data.frame(p_El)
colnames(data_El) = c("lon", "lat", "count")

Elgy_plot <- ggplot(data_El, aes(x = lon, y = lat)) + 
  geom_raster(aes(fill = count)) + 
  geom_contour(aes(z = count), colour = "black", size = 0.5, alpha = 0.5) +
  #geom_point(data = lakes[1,]) +
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colours = rev(viridis::cividis(1000)),
                       limits=c(minValue(raster_El),maxValue(raster_El))) +
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
  theme_minimal()
print(Elgy_plot)


# Khamra
raster_data_kha <- raster(nrows = 1000000, ncols = 1000000, xmn = min(Kham$lon), xmx = max(Kham$lon), ymn = min(Kham$lat), ymx = max(Kham$lat), res = .25,
                      crs = 4326)
raster_Kham <- rasterize(Kham[,c("lon", "lat")], raster_data_kha, fun = 'count')
plot(raster_Kham)
plot(lakes[2,], add = TRUE)
p_Kha <- rasterToPoints(raster_Kham)
data_Kha <- data.frame(p_Kha)
colnames(data_Kha) = c("lon", "lat", "count")

Kha_plot  <- ggplot(data = data_Kha,aes(lon, lat)) + 
  geom_raster(aes(fill = count)) + 
  geom_contour(aes(z = count), colour = "black", size = 0.5, alpha = 0.5) +
  #geom_sf(data = lakes[2,])+
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colours = rev(viridis::cividis(1000)),
                       limits=c(minValue(raster_Kham),maxValue(raster_Kham)) +
                         scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)))+
  theme_minimal()
print(Kha_plot)



# Ill
raster_data_Ill <- raster(nrows = 1000000, ncols = 1000000, xmn = min(Ill$lon), xmx = max(Ill$lon), ymn = min(Ill$lat), ymx = max(Ill$lat), res = .25,
                          crs = 4326)
raster_Ill <- rasterize(Ill[,c("lon", "lat")], raster_data_Ill, fun = 'count')
plot(raster_Ill)
plot(lakes[3,], add = TRUE) 
p_Ill <- rasterToPoints(raster_Ill)
data_Ill <- data.frame(p_Ill)
colnames(data_Ill) = c("lon", "lat", "count")

Ill_plot  <- ggplot(data = data_Ill, aes(lon, lat)) + 
  geom_raster(aes(fill = count)) + 
  geom_contour(aes(z = count), colour = "black", size = 0.5, alpha = 0.5) +
  #geom_sf(data = lakes[3,])+
  coord_sf() + 
  labs(x = NULL, y = NULL) + 
  scale_fill_gradientn(colours = rev(viridis::cividis(1000)),
                       limits=c(minValue(raster_Ill),maxValue(raster_Ill)) +
                         scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)))+
  theme_minimal()
print(Ill_plot)


