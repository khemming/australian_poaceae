  
# library 
  library(raster)
  library(tidyverse)
  library(broom)
  library(magrittr)
  
  rm(list = ls())
  
# data ----------------------------------------------------------------   
# model data
  load("data/supplementary materials/genera gls model data.RData")

# sub tribe names  
  st_names <- c("Ancient", "C3_recent", "Early_endemic", "Early_native", 
                "Homegrown_C4s", "Mid_endemic", "Mid_native")
  length(st_names)
  
# predictor variables
  pv <- read.csv("results/csv/predictor variables 1146.csv") %>%
          dplyr::select(-cell_id)
  head(pv)
  cell_id <- read.csv("results/csv/predictor variables 1146.csv") %>%
              dplyr::select(cell_id)

# subtribe iNEXT richness
  st <- read.csv("results/supplementary materials/genera/genera pv 1146.csv") %>% 
        dplyr::select(all_of(st_names))
  head(st)

# predicting distributions ------------------------------------------------
  pred_mat <- matrix(ncol = length(st_names), 
                     nrow = nrow(pv))
  
  for (i in 1:ncol(pred_mat)){
    pred_mat[,i] <- predict(model_ls[[i]], newdata = pv)
  }
  
# check
  head(pred_mat[, 4])
  
## r2 ----------------------------------------------------------------------
# compare predicted and observed distributions 
  observ <- st
  pred <- data.frame(pred_mat) 
  colnames(pred) <- names  
  head(pred) 
  
  r2 <- data.frame(sub_tribe = st_names,
                   adj_r2 = NA)
  
  for (i in 1:length(st_names)){
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
  write.csv(r2, "results/supplementary materials/genera/r2.csv", row.names = F)
  
# ----------------------------------------------------------------------
  

  