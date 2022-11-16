
  
# library -------------------------------------------------------------
  library(tidyverse)
  library(broom)
  library(magrittr)
  library(raster)
  
  rm(list = ls())
  
# data ----------------------------------------------------------------   
# model data
  load("data/rdata/models and estimates.R")
  
# australia 
  oz <- raster("C:/Users/Hemming/Dropbox/Poaceae/Data files/Australia/Australia 100 km.grd")
  
# subtribes 
  spp <- read.csv("results/csv/subtribes and predictor variables.csv") %>%
         dplyr::select(All.Andropogoneae:Native.species)

# predictor variables
  pv <- read.csv("results/csv/subtribes and predictor variables.csv") %>%
    dplyr::select(pcoldq, pwarmq, amt, ts, arid, th, clay, proportion_cover)
  head(pv)

# cell ID
  cell_id <- read.csv("results/csv/predictor variables scaled.csv", header = T) %>%
    mutate(cell_id = 1:length(oz)) %>%
    filter(cell_category_all == "land") %>%
    dplyr::select(cell_id)
  
# predicting distributions ------------------------------------------------
# matrix to contain results
  predicted <- matrix(ncol = length(spp), 
                      nrow = length(oz))
  colnames(predicted) <- names(spp)
  
  for (i in 1:length(spp)){  
    dat <- model_list[[i]]
  predicted[, i] <- predict(dat, newdata = pv, na.action = na.exclude)
    }
  
# check
  predicted <- data.frame(predicted)
  
  predicted[200:250, ]
  hist(predicted$All.Andropogoneae)
  
  
# calculate r2 ----------------------------------------------------------------------
# data frame for results
  r2 <- data.frame(model = names(spp),
                   adj_r2 = NA)
  
  adj_r2 <- function(observed, predicted){
    y <- observed
    yhat <- predicted
  
  # r2  
    r_squared = 1 - sum((y - yhat)^2, na.rm = T) / sum((y - mean(y, na.rm = T))^2, na.rm = T)
    r_squared
    
  # adjusted R2
    n <- length(na.omit(y))
    p <- 9
    adj_r_squared = 1 - (1 - r_squared) * ((n - 1)/(n - p - 1))
    return(adj_r_squared) 
  } # ajd_r2 end
  
# run 
  for (i in 1:length(spp)){
    
    r2[i] <- adj_r2(spp[i], )
  }
  

  
  write.csv(r2, "Results/csv/r2.csv", row.names = F)
  
  # -------------------------------------------------------------------------
  
