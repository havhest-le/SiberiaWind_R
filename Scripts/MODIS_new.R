rm(list = ls(all= TRUE))


library(sp)
library(sf)
library(raster)
library(rgdal)
library(rgeos)
library(ncdf4)
library(rnaturalearth)
library(tidyverse)
library(glue)
library(mapview)
library(units)

# Map
map     <- rnaturalearth::ne_coastline(scale = 50, returnclass = "sf")
ext     <- extent(c(103.82, 180, 50.07, 80.56))


#####
# Input data 
#####

# coordinates lakes
# lakes   <- read_sf("Data/lakesSHP/lakes_sf.shp")
# proj   <- glue("+proj=laea +lon_0={mean(ext[1:2])} +lat_0={mean(ext[3:4])}")
# lks    <- lakes %>% st_transform(proj)
# lkb    <- lks %>% st_buffer(100000)
# plot(lkb)

load(file = "Results/lakes_buf.RData")
Elgy <- lakes_buf[["Elgy"]]
Kham <- lakes_buf[["Khamra"]]
Ill <- lakes_buf[["Ill"]]

# MODIS
MODIS <- read.csv2("N:/bioing/data/Projekte/Fires/East Siberia/R_Data/MODIS_east_siberia.txt")
MODIS <- select(.data = MODIS,"FID", "LATITUDE","LONGITUDE", "ACQ_DATE", "FRP")
names(MODIS)[names(MODIS) == "ACQ_DATE"] <- "TIME"
MODIS$TIME <- as.POSIXct(x = MODIS$TIME, format = '%d.%m.%Y %H:%M:%S')
MODIS$FRP <- as.numeric(MODIS$FRP)
MODIS$Year <- as.numeric(format(MODIS$TIME, "%Y"))
head(MODIS)


raster_frame <- rasterFromXYZ(MODIS) %>% 
  mask(Elgy) %>% 
  as("SpatialPixelsDataFrame") %>% 
  as_data_frame()


MODIS_Elgy <- MODIS %>%
  filter(LONGITUDE >= min(Elgy) & LONGITUDE <= 180 & LATITUDE >= 50.07  & LATITUDE <= 80.56 & FRP > 0)
head(MODIS_ext)


plot(MODIS$FRP)


# Elgy
st_intersection()




ext <- as(extent(c(103.82, 180, 50.07, 80.56)), "SpatialPolygons")



#####
# Plotting
#####





