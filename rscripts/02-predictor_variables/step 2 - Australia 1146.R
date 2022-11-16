

# library ---------------------------------------------------------
  library(raster)
  library(tidyverse)
  library(rgdal)
  
  rm(list = ls())
  
# data files ------------------------------------------------------
# Australia outline
  aus_shp <- readOGR("C:/Users/s436862/Dropbox/Poaceae/Data files/Australia/Australia shapefile.shp")
  
# clay raster as template
  aus_100 <- raster("C:/Users/s436862/Dropbox/Poaceae/Results/rasters/predictor variables/clay.grd")
  
# 100 km Australia --------------------------------------------------------------------
# cut out Australia and provide cell values
  plot(aus_100)
  plot(aus_shp, add = T)
  temp <- setValues(aus_100, 1:ncell(aus_100))
  plot(temp)
  mask <- mask(temp, aus_100)
  aus_cells <- length(na.omit(getValues(aus_100)))
  aus_cells 
  plot(mask)
  writeRaster(mask, "data/Australia/Australia 1146.grd", overwrite = T)
  
# lat/long centroid coordinates of raster cells (x/y)  
  cell_list <- rasterToPoints(mask)
  cell_list <- data.frame(cell_list)
  head(cell_list)
  names(cell_list) <- c("long", "lat", "cell_id")
  
# proportion of cell covered by shapefile
  pc1 <- rasterize(aus_shp, aus_100, getCover = T)
  pc2 <- data.frame(getValues(pc1))
  
# cell_id
  cell_id <- 1:length(aus_100)
  
# cell category for all cells: ocean or land
  cell_category <- ifelse(!is.na(getValues(aus_100)), "land", "ocean")
  table(cell_category, exclude = NULL)
  
# df
  aus_df <- data.frame(cbind(cell_id, cell_category, pc2))
  colnames(aus_df) <- c("cell_id", "cell_category", "proportion_cover")
  
# add in coordinates of land-cells
  aus_df2 <- left_join(aus_df, cell_list, by = "cell_id")
  slice(aus_df2, 400:410)
  
# save  
  write.csv(aus_df2, "data/Australia/Australia 2538.csv", row.names = F)
  
# ----------------------------------------------------------------------------------
  
  