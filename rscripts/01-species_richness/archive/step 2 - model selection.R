###########################################################################
# run lowest AICc model and save results
###########################################################################

# library -----------------------------------------------------------------
  library(tidyverse)
  library(broom)
  library(magrittr)
  library(raster)
  library(MuMIn)
  library(ape)
  library(nlme)  
  
  rm(list = ls())
  
# data --------------------------------------------------------------------
# subtribes and predictor variables
  spp_pv <- read.csv("results/csv/subtribes and predictor variables.csv")
  head(spp_pv)
  
# how many sub-tribes are there?
  spp <- spp_pv %>% dplyr::select(All.Andropogoneae:Native.species)
  length(spp)
  
# correct for spatial auto-correlation ------------------------------------
# function 
  model_sel <- function(spp_col){
  
# models accounting for spatial auto-correlation
    model_e <- gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                   data = spp_pv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit)
    model_g <- gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                   data = spp_pv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit)
    model_s <- gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                   data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
    model_a <- gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                   data = spp_pv, correlation = corLin(form = ~long + lat, nugget=T), na.action = na.omit)
    model_r <- gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                   data = spp_pv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit)
    
  # compare models using AICc
    models <- model.sel(model_g, model_s , model_a, model_r, model_e)
    m_sel <- rownames(models)[1]
    return(m_sel)
  }
  
  model_df <- data.frame(spp = names(spp),
                         model_type = NA)
  
  # run function
  for (i in 1:length(spp)){
    spp_col <- spp_pv[, i]
    model_df[i, 2] <- model_sel(spp_col)
    
  }
  
  write.csv(model_df, "results/csv/model selection.csv", row.names = F)

# -------------------------------------------------------------------------------------    