######################################
# run lowest AICc model and save results
######################################

# figure out if/why models are wrong

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
# spp and variables
  spp_pv <- read.csv("results/csv/subtribes and predictor variables.csv")
  head(spp_pv)
 
# sub-tribes
  spp <- read.csv("results/csv/subtribe names.csv")
  glimpse(spp)
# lowest-AIC model
  model_type <- read.csv("results/csv/model selection.csv")
  
# run lowest AICc model and get estimates ---------------------------------
# functions for lowest AICc models
# s method
  model_s <- function(spp_col){
    m_s <- gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
               data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
      }
# r method
  model_r <- function(spp_col){
    m_r <- gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
               data = spp_pv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit)
      }    
  
# confidence estimates and intervals
  model_est <- function(model_x){data.frame(intervals(model_x, 0.95, which = "coef")$coef) }

# lists of models and parameter estimates
  model_list <- list()
  est_list <- list()
  
# (1) All Andropogoneae ---------------------------------
# model_s
  spp_col <- spp_pv$All.Andropogoneae
  model_list[[1]] <- model_s(spp_col)
  est_list[[1]] <- model_est(model_list[[1]])
  
# (2) All C3 --------------------------------------------
# model_s
  spp_col <- spp_pv$All.C3
  model_list[[2]] <- model_s(spp_col)
  est_list[[2]] <- model_est(model_list[[2]])
  
# (3) All C4 --------------------------------------------
# model_s
  spp_col <- spp_pv$All.C4
  model_list[[3]] <- model_s(spp_col)
  est_list[[3]] <- model_est(model_list[[3]])
  
# (4) All Chloridoideae ---------------------------------
# model_r
  spp_col <- spp_pv$All.Chloridoideae
  model_list[[4]] <- model_r(spp_col)
  est_list[[4]] <- model_est(model_list[[4]])

# (5) All Micrairoideae ---------------------------------
# model_r
  spp_col <- spp_pv$All.Micrairoideae
  model_list[[5]] <- model_r(spp_col)
  est_list[[5]] <- model_est(model_list[[5]])
  
# (6) All Paniceae --------------------------------------
# model_s
  spp_col <- spp_pv$All.Paniceae
  model_list[[6]] <- model_s(spp_col)
  est_list[[6]] <- model_est(model_list[[6]])
  
# (7) Endemic Andropogoneae ----------------------------
# model_r
  spp_col <- spp_pv$Endemic.Andropogoneae
  model_list[[7]] <- model_r(spp_col)
  est_list[[7]] <- model_est(model_list[[7]])
  
# (8) Endemic C3 ---------------------------------------
# model_r 
  spp_col <- spp_pv$Endemic.C3
  model_list[[8]] <- model_r(spp_col)
  est_list[[8]] <- model_est(model_list[[8]])
  
# (9) Endemic C4 ---------------------------------------
# model_s
  spp_col <- spp_pv$Endemic.C4
  model_list[[9]] <- model_s(spp_col)
  est_list[[9]] <- model_est(model_list[[9]])
  
# (10) Endemic Chloridoideae ----------------------------
# model_s
  spp_col <- spp_pv$Endemic.Chloridoideae
  model_list[[10]] <- model_s(spp_col)
  est_list[[10]] <- model_est(model_list[[10]])
  
# (11) Endemic Paniceae ---------------------------------
# model_s 
  spp_col <- spp_pv$Endemic.Paniceae
  model_list[[11]] <- model_s(spp_col)
  est_list[[11]] <- model_est(model_list[[11]])
  
# (12) Endemic species -----------------------------------
# model_s
  spp_col <- spp_pv$Endemic.species
  model_list[[12]] <- model_s(spp_col)
  est_list[[12]] <- model_est(model_list[[12]])
  
# (13) Height all ---------------------------------------
# model_s
  spp_col <- spp_pv$height
  model_list[[13]] <- model_s(spp_col)
  est_list[[13]] <- model_est(model_list[[13]])
  
# (14) Height C4 ----------------------------------------
# model_r
  spp_col <- spp_pv$height.C4
  model_list[[14]] <- model_r(spp_col)
  est_list[[14]] <- model_est(model_list[[14]])
  
# (15) Native Andropogoneae ------------------------------
# model_s 
  spp_col <- spp_pv$Native.Andropogoneae
  model_list[[15]] <- model_s(spp_col)
  est_list[[15]] <- model_est(model_list[[15]])
  

# (16) Native Chloridoideae ----------------------------
# model_r
  spp_col <- spp_pv$Native.Chloridoideae
  model_list[[16]] <- model_r(spp_col)
  est_list[[16]] <- model_est(model_list[[16]])

# (17) Native Paniceae ----------------------------------
# model_r 
  spp_col <- spp_pv$Native.Paniceae
  model_list[[17]] <- model_r(spp_col)
  est_list[[17]] <- model_est(model_list[[17]])
  
# (18) Native species ------------------------------------
# model_r
  spp_col <- spp_pv$Native.species
  model_list[[18]] <- model_r(spp_col)
  est_list[[18]] <- model_est(model_list[[18]])
  
# save  -------------------------------------------------
  save.image("data/Rdata/models and estimates.R")

# -------------------------------------------------------    