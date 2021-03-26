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

####
# Input and clean data ####
####


map     <- rnaturalearth::ne_coastline(scale = 50, returnclass = "sf")
ext     <- extent(c(103.82, 180, 50.07, 80.56))
mapCrop <- map %>% 
  st_intersection(st_as_sf(as(ext, "SpatialPolygons")) %>% 
                      st_set_crs(4326)) %>% st_geometry 


# Elgygytgen
Elgy <- read_sf("Data/Lakes/Elgygytgen/Lake_area.shp")
Elgy <- st_transform(Elgy, 4326)
El_buf <- st_transform(Elgy, crs = CRS("+proj=laea")) %>%
  st_buffer(400000) %>% st_transform(4326)
El_plus_buffer <- data.frame(Elgy, El_buf) %>%
  select("geometry", "geometry.1") 
names(El_plus_buffer)[1:2] <- c("lake", "buffer")
head(El_plus_buffer)
plot(mapCrop)
plot(El_plus_buffer$buffer, add = TRUE)
plot(El_plus_buffer$lake, add = TRUE)

# Khamra
Khamra <- read_sf("Data/Lakes/Khamra/Khamra_polygon.shp")
Khamra <- st_transform(Khamra, 4326)
Kha_buf <- st_transform(Khamra, crs = CRS("+proj=laea")) %>%
    st_buffer(400000) %>% st_transform(4326)
Ka_plus_buffer <- data.frame(Khamra, Kha_buf) %>%
    select("geometry", "geometry.1") 
names(Ka_plus_buffer)[1:2] <- c("lake", "buffer")
head(Ka_plus_buffer)
plot(Ka_plus_buffer$buffer, add = TRUE)
plot(Ka_plus_buffer$lake, add = TRUE)

# Illerney
Ill <- read_sf("Data/Lakes/Illerney/Umriss_Polygon_Ilriney.shp")
Ill <- st_transform(Ill, 4326)
Ill_buf <- st_transform(Ill, crs = CRS("+proj=laea")) %>%
  st_buffer(400000) %>% st_transform(4326)
Ill_plus_buffer <- data.frame(Ill, Ill_buf) %>%
  select("geometry", "geometry.1") 
names(Ill_plus_buffer)[1:2] <- c("lake", "buffer")
head(Ill_plus_buffer)
plot(Ill_plus_buffer$buffer, add = TRUE)
plot(Ill_plus_buffer$lake, add = TRUE)


