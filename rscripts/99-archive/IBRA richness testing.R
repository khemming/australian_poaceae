################################################################################
# richness among IBRA regions
################################################################################

# library ----------------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(tidyverse)
 
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
  
# test1: using aus raster ---------------------------------------------------
# remove duplicate records from each cell
# i.e. spp represented by only 1 record per cell
  xy <- recs %>% dplyr::select(longitude, latitude)
  recs$cell_id <- extract(aus, xy)
 
  spp <- as.character(recs$species)
  hgt <- rasterize(ibra, aus, field = spp, fun = mean)
  plot(hgt)  
  
# let's do an IBRA foirst
  test1 <- rasterize(ibra, aus, fun = "first")
  plot(test1) # cool
  
# test2: package sf ----------------------------------------------------------------------------  
  library(sf)
  
  map <- ibra
  pnts <- ibra$
  names(pnts) <- c("x", "y")
  
  pnts_sf <- st_as_sf(pnts, coords = c('y', 'x'), crs = st_crs(map))
  
  pnts <- pnts_sf %>% mutate(
    intersection = as.integer(st_intersects(geometry, map))
    , area = if_else(is.na(intersection), '', map$REG_CODE_7[intersection])
  ) 
  
  head(pnts)
  
  
  head(map$REG_CODE_7)
  
  
   
  
  
  
# test3: over function --------------------------------------------------------------------------
# get xy of each record
  xy <- recs %>% dplyr::select(longitude, latitude)
# over function requires it as SpatialPoints
  xy_sp <- SpatialPoints(xy, crs(aus))
  
# attempt 1  
  test_o1 <- over(xy_sp, ibra) # works
# gives us the number of records per IBRA region
  names <- test_o1$REG_NAME_7
  table(names)
  
# summarise spp number per IBRA region
  spp <- as.numeric(factor(recs$species))
  a <- over(xy_sp, ibra, fn = length)    # ran but did not finish
  
# test4: different method for package sf ---------------------------------------------------------  
  library(magrittr)
  library(ggplot2)
  library(sf)

# get xy of each record
  xy <- recs %>% dplyr::select(longitude, latitude)

# IBRA regions using sf
  i_sf <- read_sf("data/IBRA regions/IBRA shapefile.shp")
  plot(i_sf) # cool
  
# subset names (maybe don't need to do?)  
  tt1 <- i_sf %>% dplyr::select(REG_NAME_7, geometry)
  plot(tt1)
  
# xy to pnts
  pnts <- xy %>% mutate(x = longitude,
                        y = latitude) %>%
          dplyr::select(x, y)
  
  head(pnts)
  
  pnts_s <- pnts %>% sample_n(500, replace = F)
  
pnts_s$region <- apply(pnts_s, 1, function(row) {  
  # transformation to palnar is required, since sf library assumes planar projection 
  tt1_pl <- st_transform(tt1, 2163)   
  coords <- as.data.frame(matrix(row, nrow = 1, 
                                 dimnames = list("", c("x", "y"))))   
  pnt_sf <- st_transform(st_sfc(st_point(row),crs = 4326), 2163)
  # st_intersects with sparse = FALSE returns a logical matrix
  # with rows corresponds to argument 1 (points) and 
  # columns to argument 2 (polygons)
  
  tt1_pl[which(st_intersects(pnt_sf, tt1_pl, sparse = FALSE)), ]$REG_NAME_7 
})

head(pnts_s)  


  # test5: MIGHT WORK another different method for package sf ------------------------------------------------
  library(magrittr)
  library(ggplot2)
  library(sf)

# get xy of each record
  coord <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
  xy <- recs %>% dplyr::select(longitude, latitude)
  dsf <- sf::st_as_sf(xy, coords=c("longitude","latitude"), crs= coord)
  map <- st_as_sf(ibra, crs = coord)
  xy_sp <- SpatialPoints(xy, crs(coord))

# find country of each coordinate:
  int <- sf::st_intersects(dsf, map, sparse = FALSE)
  int_df <- data.frame(int)
  colnames(int_df) <- map$REG_CODE_7
  head(int_df)
  
  nn <- colnames(int_df)
  
  ibra_list <- apply(int_df,1, function(x) nn[x])
  head(ibra_list)
  length(recs$ibra_code)
  head(recs)
  
  k <- unlist(j)
  head(k)
  
  ibra_list[sapply(ibra_list, function(x) length(x)==0L)] <- NA
  ibra_list[sapply(ibra_list, is.null)] <- NA
  recs$ibra_code <- unlist(ibra_list)
  head(recs)
  glimpse(recs)
  table(recs$ibra_code, )
  
  
  int_ul <- unlist(int)
  int_df2 <- data.frame(int)
  xy$sub_region <- as.character(map$REG_NAME_7[unlist(int)])
  xy
  head(xy)  
  
  int_df <- data.frame(int)
  reg_id <- as.character(map$REC_ID)
  int_df2 <- left_join(int_df, reg_id, by = "")
  
  
  head(reg_id)
# ------------------------------------------------------------------------------------------------  
  coord <- "+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs"
  xy <- recs %>% dplyr::select(longitude, latitude)
  pnts <- xy %>% mutate(x = longitude,
                        y = latitude) %>%
    dplyr::select(x, y)
  map <- st_as_sf(ibra, crs = coord)
  glimpse(map)
  map$REG_NAME_7 <- as.character(map$REG_NAME_7)
  pnts_sf <- st_as_sf(pnts, coords = c('y', 'x'), crs = st_crs(map))
  
  pnts_j <- pnts_sf %>% mutate(
    intersection = as.integer(st_intersects(geometry, map))
    , area = if_else(is.na(intersection), '', map$REG_NAME_7[intersection])
  ) 
  
  head(pnts_j)
  table(pnts_j$intersection, exclude = NULL)
  
# ----------------------------------------------------------------------------------------------  