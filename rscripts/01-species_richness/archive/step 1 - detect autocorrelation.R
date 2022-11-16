

# library ------------------------------------------------------------------
  library(tidyverse)
  library(broom)
  library(magrittr)
  library(raster)
  library(MuMIn)
  library(ape)
  library(nlme)  
  library(car)
  library(rgdal)
  
  rm(list = ls())
  
# data --------------------------------------------------------------------
  spp_pv <- read.csv("results/csv/subtribes and predictor variables.csv")
  head(spp_pv)
  
# sub-tribes
  spp <- spp_pv %>% dplyr::select(All.Andropogoneae:Native.species)
  length(spp)

# identify spatial auto-correlation in data -------------------------------
# store results
  model_mat <- matrix(ncol = 3, 
                      nrow = length(spp))
  rownames(model_mat) <- names(spp)
  colnames(model_mat) <- c("Moran_p", "model_method", "AICc")
  
# detecting spatial autocorrelation  
  for (i in 1:length(spp)) {
  
  # columns of interest
    spp_col <- spp_pv[i] # / [1] / [i]
   
  # Moran's I
    xy <- spp_pv %>% filter(!is.na(spp_col)) %>%
      dplyr::select(all_of(i), long, lat)
    head(xy)
    geo <- cbind(xy$long, xy$lat)
    samples.dist <- as.matrix(dist(geo))
    dim(samples.dist)
    samples.dist.inv <- 1/samples.dist
    diag(samples.dist.inv) <- 0
    m_i <- Moran.I(xy[, 1], samples.dist.inv, alternative = "greater", na.rm = TRUE) 
    ac_list[[i]] <- m_i
  } # ac finish
  
  ac_list # all p-values are zero -- spatial auto-correlation for all sub-tribes
  
  saveRDS(ac_list, "data/Rdata/Moran's I.RDS")
  
# -----------------------------------------------------------------------  
