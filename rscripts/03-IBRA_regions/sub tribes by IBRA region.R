

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
  crs(ibra) <- crs(aus)
  
# sub regions
  sub <- readOGR("data/IBRA regions/IBRA subregion shapefile.shp")
  crs(sub) <- crs(aus)

# records
  recs <- read.csv("data/master data/master grass records.csv") %>%
          dplyr::select(species, latitude, longitude)

# tranfrom data to sf-friendly format
  coord <- crs(aus)
  xy <- recs %>% dplyr::select(longitude, latitude)
  dsf <- sf::st_as_sf(xy, coords=c("longitude","latitude"), crs = coord)
  xy_sp <- SpatialPoints(xy, crs(coord))
  ibra_map <- st_as_sf(ibra, crs = coord) # ibra regs
  sub_map <- st_as_sf(sub, crs = coord)  # sub regs
  
# assign record to IBRA region code -------------------------------------------
  int <- sf::st_intersects(dsf, ibra_map, sparse = FALSE) # TRUE/FALSE matrix
  nn <- ibra_map$REG_CODE_7
  ibra_list <- apply(int, 1, function(x) nn[x])
  head(ibra_list)
  ibra_list[sapply(ibra_list, function(x) length(x) == 0L)] <- NA # not all records are in a region 
  recs$ibra_code <- unlist(ibra_list) # region number
  glimpse(recs)

# join ibra names and codes to records
  ibra_codes <- data.frame(ibra_map) %>% 
                mutate(ibra_name = as.character(REG_NAME_7),
                       ibra_code = as.character(REG_CODE_7),
                       ibra_no   = REG_NO_61) %>%
                  dplyr::select(ibra_name, ibra_code, ibra_no)
  recs2 <- left_join(recs, ibra_codes, by = "ibra_code")
  head(recs2) 
  
# assign record to sub region code -----------------------------------------
  int <- sf::st_intersects(dsf, sub_map, sparse = FALSE) # TRUE/FALSE matrix
  nn <- sub_map$SUB_CODE_7
  sub_list <- apply(int, 1, function(x) nn[x])
  head(sub_list)
  sub_list[sapply(sub_list, function(x) length(x) == 0L)] <- NA # not all records have region 
  recs2$sub_code <- unlist(sub_list) # subregion number
  glimpse(recs2)
  
# join sub names and codes to records
  sub_codes <- data.frame(sub_map) %>% 
              mutate(sub_name = as.character(SUB_NAME_7),
                     sub_code = as.character(SUB_CODE_7),
                     sub_no   = SUB_NO_61_) %>%
       dplyr::select(sub_name, sub_code, sub_no)
  recs3 <- left_join(recs2, sub_codes, by = "sub_code")
  head(recs3)
  
  write.csv(recs3, "data/master data/native grass records by IBRA region.csv", row.names = F)

# --------------------------------------------------------------------------------  
  
  
  
  
  
    