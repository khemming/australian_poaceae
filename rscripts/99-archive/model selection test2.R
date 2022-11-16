####################################################################
# gls model selection
####################################################################

# library ------------------------------------------------------------------
  library(tidyverse)
  library(broom)
  library(magrittr)
  library(raster)
  library(MuMIn)
  library(ape)
  library(nlme)  
  
  rm(list = ls())
 
# raster layers ----------------------------------------------------------------   
  setwd("C:/Users/s436862/Dropbox/Projects/Sue Bryceson/Results/Rasters")
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  spp <- as.data.frame(c.stack, na.rm = F)
  glimpse(spp)
  setwd("C:/Users/s436862/Dropbox/Projects/Sue Bryceson")

# environmental variables
  pv <- read.csv("Results/predictor variables/predictor variables scaled.csv")
  head(pv)

# bind spp and EV data and subset to terrestrial only cells   
  spp_pv <- cbind(spp, pv) %>%
            filter(cell_category_all == "land") %>%
            dplyr::select(-cell_category_all)
  glimpse(spp_pv)
  
# identify spatial autocrrelation in data -----------------------------------------------
  ac_list <- list()
  
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
  
  ac_list # all p-values are zero -- spatial autocrrelation everywhere
  
  saveRDS(ac_list, "data/Rdata/Moran's I.RDS")
  
# correct for spatial autocorrelation ------------------------------------------------------
# function 
  model_sel <- function(spp_col){
  # linear model
    model_lm <- lm(spp_col ~ clay+ th + pcoldq + pwarmq + ts + arid + amt +  proportion_cover, 
      data = spp_pv) 
  # models accounting for spatial autocorrelation
    model_e <- gls(spp_col ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
      data = spp_pv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit)
    
    model_g <- gls(spp_col ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
      data = spp_pv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit)
    
    model_s <- gls(spp_col ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
      data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
    
    model_a <- gls(spp_col ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
      data = spp_pv, correlation = corLin(form = ~long + lat, nugget=T), na.action = na.omit)
    
    model_r <- gls(spp_col ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
      data = spp_pv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit)
    
  # compare models using AICc
    models <- model.sel(model_lm, model_g, model_s , model_a, model_r)
    m_sel <- rownames(models)[1]
    return(m_sel)
  }
  
  model_type <- data.frame(spp = names(spp),
                             model_type = NA)
  
# run function
  for (i in 1:length(spp)){
    
    which_model_to_run[i, 2] <- model_sel(spp_pv[, i])
    
  }
  
  write.csv(which_model_to_run, "results/csv/model selection.csv")
  
  write.csv(spp_pv, "results/csv/subtribes and predictor variables.csv", row.names = F)
  
# -----------------------------------------------------------------------------