

# library ----------------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(dplyr)
  library(rmapshaper)
  library(rgeos)
  
  rm(list = ls())

# data -------------------------------------------------------------------------
# IBRA regions
  ibra <- readOGR("data/IBRA regions/ibra7_regions.shp")
  plot(ibra)

# IBRA subregions 
  ibra_sr <- readOGR("data/IBRA regions/IBRA7_subregions_states.shp")
  ibra_sr
  
# australia 
  raster <- raster("data/Australia/Australia 1 km.grd")
  
# simplify regions ---------------------------------------------------------------
  ibra_sim <- ms_simplify(ibra, keep = .05, # proportion of points
                       weighting = 0.7) # smoothing index
  crs(ibra_sim) <- "+init=epsg:3577 +proj=aea +lat_1=-7 +lat_2=-45 +lat_0=112 +lon_0=155 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  writeOGR(ibra_sim, ".", "data/IBRA regions/IBRA shapefile", driver = "ESRI Shapefile")
  
# simplify subregions ------------------------------------------------------------
  ibra_sr_sim <- ms_simplify(ibra_sr, keep = .05, # proportion of points
                          weighting = 0.7) # smoothing index
  plot(ibra_sr_sim)
  
  crs(ibra_sr_sim) <- "+init=epsg:3577 +proj=aea +lat_1=-7 +lat_2=-45 +lat_0=112 +lon_0=155 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  plot(ibra_sr_sim)
  
  writeOGR(ibra_sr_sim, ".", "data/IBRA regions/IBRA subregion shapefile", driver = "ESRI Shapefile")
# -------------------------------------------------------------