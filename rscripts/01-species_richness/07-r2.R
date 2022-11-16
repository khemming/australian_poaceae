  
# library 
  library(raster)
  library(tidyverse)
  library(broom)
  library(magrittr)
  library(MuMIn)
  library(ape)
  library(nlme)  
  
  rm(list = ls())
  
# data ----------------------------------------------------------------   
# model data
  load("data/rdata/models.RData")

# sub tribe names  
  st_names <- read.csv("results/csv/subtribe names.csv")
  nrow(st_names)
  
# predictor variables
  pv <- read.csv("results/csv/predictor variables 1146.csv") %>%
          dplyr::select(-cell_id)
  head(pv)
  cell_id <- read.csv("results/csv/predictor variables 1146.csv") %>%
              dplyr::select(cell_id)

# subtribe iNEXT richness
  st <- st_pv %>%
          dplyr::select(1:nrow(st_names))
  
# predicting distributions ------------------------------------------------
  pred_mat <- matrix(ncol = nrow(st_names), 
                     nrow = nrow(pv))
  
  for (i in 1:ncol(pred_mat)){
    pred_mat[,i] <- predict(model_ls[[i]], newdata = pv)
  }
  
# check
  pred_mat
  head(pred_mat[, 5])
  
## r2 ----------------------------------------------------------------------
# compare predicted and observed distributions 
  observ <- st
  pred <- data.frame(pred_mat) 
  colnames(pred) <- st_names$names  
  head(pred) 
  
  r2 <- data.frame(sub_tribe = st_names[, 1],
                   adj_r2 = NA)
  
  for (i in 1:nrow(st_names)){
    y <- observ[, i]
    yhat <- pred[, i]
    
    r_squared = 1 - sum((y - yhat)^2, na.rm = T) / 
      sum((y - mean(y, na.rm = T))^2, na.rm = T)
    r_squared
    
  # adjusted R2
    n <- length(na.omit(y))
    p <- 9
    r2[i, 2] = 1 - (1 - r_squared) * ((n - 1)/(n - p - 1))
  # round
    r2[i, 2] <- round(r2[i, 2], 3)
  } # fun end
  r2
  
# save 
  write.csv(r2, "results/csv/r2.csv", row.names = F)
  
  # ----------------------------------------------------------------------
