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


data_coord <- data.frame(location = c("Khamra","Illirney","Rauchagytgyn", "Elgygytgyn"),
                         lon = c(112.98, 167.57, 168.71, 172.15), lat = c(59.99, 67.15, 67.80, 67.58))
map     <- rnaturalearth::ne_coastline(scale = 50, returnclass = "sf")
ext     <- extent(c(103.82, 180, 50.07, 80.56))

## I created a simple sf polygon collection with the lakes
## We will create the buffer later
lakes   <- read_sf("Data/lakesSHP/lakes_sf.shp") 

####
# Input data ####
####

# for Linux4 Server
files <- list.files(path = "/Volumes/potsdam/data/bioing/data/Data_Reanalyse/ERA5/Wind_u_v_pressureLevels", pattern = "*.nc", all.files = T, full.names = T)

# for vivis pc
# files <- list.files(path = "Z:/data/bioing/data/Data_Reanalyse/ERA5/Wind_u_v_pressureLevels", pattern = "*.nc", all.files = T, full.names = T)

fls_Tab <- do.call("rbind", lapply(files, function(x) {
  nf <- nc_open(x)
  tms <- as.POSIXct(nf$var[[1]]$dim[[4]]$vals*60*60, origin = "1900-01-01")
  out <- data.frame(path = x, date = tms, id = 1:length(tms))
  nc_close(nf)
  out
}))


for(y in 1979:2020) {
  
  for(m in 4:8) {
  
  cat(glue("\ryear {y} month {m}"))
    
  subTab <- subset(fls_Tab, as.numeric(format(date, "%Y")) %in% y &
                     as.numeric(format(date, "%m")) %in% m)
   
  # Creating a list for every level
  rasterList <- lapply(unique(subTab$path), function(x) {
    
    # x <- unique(subTab$path)[[1]]
    
    levelList <- lapply(1:7, function(level) {
      u <- raster::crop(brick(as.character(x), varname=  "u", level = level), ext)
      v <- raster::crop(brick(as.character(x), varname = "v", level = level), ext)
      list(u, v)
    })
    

    lapply(1:nlayers(levelList[[1]][[1]]), function(dts) {
      
      levTmp <- lapply(1:7, function(level) {
        list(levelList[[level]][[1]][[dts]],
             levelList[[level]][[2]][[dts]])
      })
      
     uDate <- calc(do.call("brick", lapply(levTmp, function(y) y[[1]])), median, na.rm = T)
     vDate <- calc(do.call("brick", lapply(levTmp, function(y) y[[2]])), median, na.rm = T)
     
     list(uDate, vDate)
    })
    
  })
  # save(rasterList, file = "~/Documents/subsave.RData")
  
  # Creating a brick for wind direction and speed 
  uBrick <- brick(lapply(rasterList, function(x) brick(lapply(x, function(y) y[[1]]))))
  vBrick <- brick(lapply(rasterList, function(x) brick(lapply(x, function(y) y[[2]]))))
  
  proj   <- glue("+proj=laea +lon_0={mean(ext[1:2])} +lat_0={mean(ext[3:4])}")
  crds   <- data.frame(coordinates(uBrick))
  pts    <- st_as_sf(crds, coords = c("x", "y"), crs = 4236) %>% st_transform(proj)
  lks    <- lakes %>% st_transform(proj)
  lkb    <- lks %>% st_buffer(700000)
  ind    <- apply(st_intersects(pts, lkb, sparse = F), 1, any)
  
  trackPts <- st_coordinates(pts)[ind,]
  endPts   <- crds[ind,]
  
  # plot(projectRaster(uBrick[[1]], crs = CRS(proj)))
  # plot(lakes %>% st_transform(proj), add = T)
  # points(trackPts, pch = 16, cex = 0.2)
  
  crdsTab <- do.call("rbind", lapply(1:nlayers(uBrick), function(z) {

    wBrick <- projectRaster(brick(uBrick[[z]], vBrick[[z]]), crs = CRS(proj))
    
    for(i in 1:24) {
      if(i==1) {
        extrWnd <- raster::extract(wBrick, trackPts)
        windTab <- data.table(tm = i, id = 1:nrow(trackPts), lon = trackPts[,1], lat = trackPts[,2], 
                              u = extrWnd[,1], v = extrWnd[,2])
      } else {
        new <- with(windTab[windTab$tm==(i-1) & !is.na(windTab$v),], 
                    data.table(tm  = i, id = id, lon = lon+u*60*60, lat = lat+v*60*60))
        new[, c('u', 'v') := data.table(raster::extract(wBrick, new[,c("lon", "lat")]))]
        windTab <- rbind(windTab, new)      
      }   
    }
    
    windTab <- windTab[windTab$id%in%which(sapply(unique(windTab$id), function(p) sum(windTab$id==p))>2),]
    
    # plot(projectRaster(uBrick[[1]], crs = CRS(proj)))
    # with(windTab[windTab$id==1,], lines(lon, lat))
    
    tracks_sf <- sfheaders::sf_linestring(
      obj = windTab[!is.na(windTab$lon),c("lon", "lat", "id")][order(windTab$id),],
      x = "lon",
      y = "lat",
      linestring_id = "id"
    ) %>% st_set_crs(proj)

    
    ### intersect
    out <- do.call("rbind", lapply(seq(0, 5000, 500), function(d) {
      inters <- st_intersects(lks %>% st_buffer(d), tracks_sf, sparse = F)
      # plot(lks[1,] %>% st_buffer(550000) %>% st_geometry())
      # points(trackPts, cex = 0.2, pch = 16)
      # plot(tracks_sf[inters[1,],], add = T)
      # plot(lks[1,] %>% st_buffer(d), add = T)
      # points(outTmp[,c("X", "Y")], pch = 21)
      do.call("rbind", lapply(1:nrow(inters), function(x) {
        if(sum(inters[x,])>0) {
        outTmp <- project(as.matrix(data.table(st_coordinates(tracks_sf[inters[x,],]))[, head(.SD,1), by = "L1"][,c("X", "Y")]), proj, inv = T)
        if(nrow(outTmp)>0) data.frame(lake = lakes$lake[x], date = subTab$date[z], lon = outTmp[,1], lat = outTmp[,2], buffer = d)
        } else NULL
      }))
    }))
    
    out[!duplicated(glue("{out[,1]}_{out[,2]}_{out[,3]}")),]
    
  }))
  
  if(file.exists("~/Documents/crdsTab.csv")) {
    write.csv(crdsTab, "~/Documents/crdsTab.csv")
  } else write.csv(crdsTab, "~/Documents/crdsTab.csv", append = TRUE)
  
  }
  
}

### output plot
plot(uBrick[[1]])
points(crdsTab[,c("lon", "lat")], pch = 16, cex = 0.4)
plot(lakes$geometry, lwd = 13, add = T, col = "red")

## test
# r0 <- raster(extent(as(lakes[1,] %>% st_transform(proj) %>% st_buffer(310000) %>% st_transform(4326), "Spatial")),
#              res = .5, crs = 4326)
# 
# rLake <- rasterize(crdsTab[,c("lon", "lat")], r0, fun = 'count')
# plot(rLake)
# plot(lakes[1,], add = T)
