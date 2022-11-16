################################################################################
# records among IBRA regions
################################################################################

# library ----------------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(tidyverse)
  library(sf)

  rm(list = ls())

# data -------------------------------------------------------------------------
# australia 
  aus <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/Australia/Australia 100 km.grd")

# IBRA regions
  ibra <- readOGR("data/IBRA regions/IBRA shapefile.shp")
  crs(ibra) <- crs(aus) # don't know why, but this needs to occur

# records
  recs <- read.csv("data/master data/native grass records.csv") %>%
          dplyr::select(species, c_hgt, latitude, longitude)

# tranfrom record and shapefile data to sf-friendly formats  
  coord <- crs(aus)
  xy <- recs %>% dplyr::select(longitude, latitude)
  dsf <- sf::st_as_sf(xy, coords=c("longitude","latitude"), crs = coord)
  map <- st_as_sf(ibra, crs = coord) 
  xy_sp <- SpatialPoints(xy, crs(coord))

# assign record to IBRA region code
  int <- sf::st_intersects(dsf, map, sparse = FALSE) # TRUE/FALSE matrix
  nn <- map$REG_CODE_7
  ibra_list <- apply(int,1, function(x) nn[x])
  head(ibra_list)
  ibra_list[sapply(ibra_list, function(x) length(x)==0L)] <- NA # not all records have region (weird)  
  recs$ibra_code <- unlist(ibra_list)

  ibra_codes <- data.frame(ibra) %>% mutate(ibra_name = as.character(REG_NAME_7),
                               ibra_code = as.character(REG_CODE_7)) %>%
                  dplyr::select(ibra_name, ibra_code)
  recs2 <- left_join(recs, ibra_codes, by = "ibra_code")
  head(recs2) 
  
  write.csv(recs2, "data/master data/native grass records by IBRA region.csv", row.names = F)

# --------------------------------------------------------------------------------  
  
  
  
  
  
    