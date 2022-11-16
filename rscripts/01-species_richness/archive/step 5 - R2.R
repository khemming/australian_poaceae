
 
# library ------------------------------------------------------------------
  library(raster)
  
  rm(list = ls())
  
# data ---------------------------------------------------------------------   
# model data
  load("data/Rdata/model estimates.R")
  
# r2 ----------------------------------------------------------------------
  pred_vals <- list()
  
  pred_vals[[i]] <- predict()
  
  adj_r2 <- function(model, observed_spp){
    
    yhat <- predict(model, newdata = )
    
    y <- observed_spp
    yhat <- getValues(pred_raster)
    r_squared = 1 - sum((y - yhat)^2, na.rm = T) / sum((y - mean(y, na.rm = T))^2, na.rm = T)
    r_squared
    
  # adjusted R2
    n <- length(na.omit(y))
    p <- 9
    adj_r_squared = 1 - (1 - r_squared) * ((n - 1)/(n - p - 1))
    return(adj_r_squared) 
  } # ajd_r2 end
  
# run 
  r2 <- data.frame(model = c("n_c3_r2",  "n_c4_r2",  "n_tot_r2",
                             "nn_c3_r2", "nn_c4_r2", "nn_tot_r2"),
                   adj_r2 = NA)
  
# doing this for native species as the predicted raster for non-natives    
  r2$adj_r2[1] <- adj_r2(native_C3, native_C3_predicted)
  r2$adj_r2[2] <- adj_r2(native_C4, native_C4_predicted)
  r2$adj_r2[3] <- adj_r2(native_total, native_total_predicted)  
  
  r2$adj_r2[4] <- adj_r2(nonnative_C3, native_C3_predicted) # add "non" onto these to reverse
  r2$adj_r2[5] <- adj_r2(nonnative_C4, native_C4_predicted)
  r2$adj_r2[6] <- adj_r2(nonnative_total, native_total_predicted)  
  
  r2
  
  write.csv(r2, "Results/csv/r2.csv", row.names = F)
  
# -------------------------------------------------------------------------