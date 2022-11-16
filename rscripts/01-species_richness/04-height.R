

# library --------------------------------------------------
  library(dplyr)
  library(raster)

  rm(list = ls())
  
# data -----------------------------------------------------
# records
  dat <- read.csv("data/ALA records/master grass records.csv")
  
# Australia
  aus <- raster("data/Australia/Australia 1136.grd")
  
# height rasters -------------------------------------
# function --------------
  hgt_fun <- function(dat) {
  hc4 <- dat %>% 
  mean(hc4$hgt, na.rm = T)
  hist(hc4$hgt)
  c4xy <- cbind(hc4$longitude, hc4$latitude)
  hgt <- as.numeric(factor(hc4$hgt))
  hc4r <- rasterize(c4xy, aus, field = hgt, fun = function(x, ...) {mean(na.omit(x)) })
  plot(hc4r)

# identity cells with >= 15 records and set as NA
  hc4_c <- rasterize(c4xy, aus, field = hgt, fun = function(x, ...) {length(na.omit(x)) })
  hc4_c[hc4_c < 15] <- NA
  plot(hc4_c)
# remove NA cells from mean height raster  
  hc4r2 <- mask(hc4r, hc4_c)
  plot(hc4r2)
  return(hc4r2)
  }
# -------------------------  
# (1) C4
  c4 <- dat %>% filter(pp == "C4")
  j <- hgt_fun(c4)
  j
  writeRaster(j, "results/rasters/Height C4.grd", overwrite = T)
  
# (2) all species 
  j <- hgt_fun(dat)
  writeRaster(j, "results/rasters/Height All.grd", overwrite = T)
  
# (4) AEM C3
  aem <- dat %>% filter(ip != "Recent")
  j <- hgt_fun(aem)
  writeRaster(j, "results/rasters/Height AEM.grd", overwrite = T)
  
# (5) Recent C3
  rec <- dat %>% filter(ip == "Recent")
  j <- hgt_fun(rec)
  writeRaster(j, "results/rasters/Height Recent.grd", overwrite = T)
  
# ---------------------------------------------------------    