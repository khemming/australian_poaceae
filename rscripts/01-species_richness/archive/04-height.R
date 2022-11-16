

# library --------------------------------------------------
  library(dplyr)
  library(raster)

  rm(list = ls())
  
# data -----------------------------------------------------
# records
  dat <- read.csv("data/ALA records/master grass records.csv")
  
# Australia
  aus <- raster("data/Australia/Australia 1136.grd")
  
# height maps ---------------------------------------------
# (1) C4
  hc4 <- dat %>% filter(pp == "C4")
  mean(hc4$hgt, na.rm = T)
  hist(hc4$hgt)
  c4xy <- cbind(hc4$longitude, hc4$latitude)
  hgt <- as.numeric(factor(hc4$hgt))
  hc4r <- rasterize(c4xy, aus, field = hgt, fun = function(x, ...) {mean(unique(na.omit(x))) })
  plot(hc4r)
  
# (2) all species 
  hall <- dat
  mean(hall$hgt, na.rm = T)
  hist(hall$hgt)
  boxplot(hgt, data = hall)
  allxy <- cbind(hall$longitude, hall$latitude)
  hgt <- as.numeric(factor(hall$hgt))
  hallr <- rasterize(allxy, aus, field = hgt, fun = function(x, ...) {mean(unique(na.omit(x))) })
  plot(hallr)
  
# (3) neo C3
## paleo are all non-neo C3s  
  Paleo_C3 <- c("Anisopogon",
                "Amphibromus",
                "Anthoxanthum",
                "Dichelachne",
                "Echinopogon",
                "Pentapogon",
                "Ancistragrostis",
                "Hookerochloa",
                "Saxipoa",
                "Sylvipoa",
                "Dryopoa",
                "Micraira",
                "Coelachne",
                "Isachne")
  hnc3 <- dat %>%
          filter(pp == "C3",
                 !genus %in% Paleo_C3)
  ncxy <- cbind(hnc3$longitude, hnc3$latitude)
  hgt <- as.numeric(factor(hnc3$hgt))
  hnc3r <- rasterize(ncxy, aus, field = hgt, fun = function(x, ...) {mean(unique(na.omit(x))) })
  plot(hnc3r)
  
# (4) paleo C3
  hpc3 <- dat %>%
    filter(genus %in% Paleo_C3)
  npxy <- cbind(hpc3$longitude, hpc3$latitude)
  hgt <- as.numeric(factor(hpc3$hgt))
  hpc3r <- rasterize(npxy, aus, field = hgt, fun = function(x, ...) {mean(unique(na.omit(x))) })
  plot(hpc3r)
  
# (5) recent C3
  hrc3 <- dat %>%
          filter(ip == "Recent" &
          pp == "C3")
  npxy <- cbind(hrc3$longitude, hrc3$latitude)
  hgt <- as.numeric(factor(hrc3$hgt))
  hrc3r <- rasterize(npxy, aus, field = hgt, fun = function(x, ...) {mean(unique(na.omit(x))) })
  plot(hpc3r)
  
# save rasters
  writeRaster(hallr, "results/rasters/Height All.grd", overwrite = T)
  writeRaster(hc4r, "results/rasters/Height C4.grd", overwrite = T)
  writeRaster(hnc3r, "results/rasters/Height Neo C3.grd", overwrite = T)
  writeRaster(hpc3r, "results/rasters/Height Paleo C3.grd", overwrite = T)
  writeRaster(hrc3r, "results/rasters/Height Recent C3.grd", overwrite = T)

# ---------------------------------------------------------    