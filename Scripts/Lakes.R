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
Elgy <- read_sf("Data/Lakes/Elgygytgen/Lake_area.shp") %>% st_transform(4326)
proj <- glue("+proj=laea +lon_0={st_coordinates(st_centroid(Elgy))[,1]} +lat_0={st_coordinates(st_centroid(Elgy))[,2]}") # Ely is the central point
El_buf <- st_transform(Elgy, crs = CRS(proj)) %>%
  st_buffer(400000) %>% st_transform(4326)
plot(mapCrop)
plot(Elgy$geometry, col = "blue", add = T)
plot(El_buf$geometry, add = T)


# Khamra
Khamra <- read_sf("Data/Lakes/Khamra/Khamra_polygon.shp") %>% st_transform(4326)
proj <- glue("+proj=laea +lon_0={st_coordinates(st_centroid(Khamra))[,1]} +lat_0={st_coordinates(st_centroid(Khamra))[,2]}")
Kha_buf <- st_transform(Khamra, crs = CRS(proj)) %>%
    st_buffer(400000) %>% st_transform(4326)
plot(Khamra$geometry, col = "blue", add = T)
plot(Kha_buf$geometry, add = T)


# Illerney
Ill <- read_sf("Data/Lakes/Illerney/Umriss_Polygon_Ilriney.shp") %>% st_transform(4326)
proj <- glue("+proj=laea +lon_0={st_coordinates(st_centroid(Ill))[,1]} +lat_0={st_coordinates(st_centroid(Ill))[,2]}")
Ill_buf <- st_transform(Ill, crs = CRS(proj)) %>%
  st_buffer(400000) %>% st_transform(4326)
plot(Ill$geometry, col = "blue", add = T)
plot(Ill_buf$geometry, add = T)

lakes <- list(Elgy = list(lake = Elgy, buffer = El_buf)
              Khamra = ())

save(lakes, file = "Results/lakes.RData")

