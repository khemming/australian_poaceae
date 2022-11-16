# linear models for now


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


# spp and variables
spp_pv <- read.csv("results/csv/subtribes and predictor variables.csv")
head(spp_pv)

# sub-tribes
spp <- spp_pv %>% dplyr::select(All.Andropogoneae:Native.species)
length(spp) 
glimpse(spp)
# lowest-AIC model
model_type <- read.csv("results/csv/model selection.csv")

# run lowest AICc model and get estimates ---------------------------------
# functions for lowest AICc models
# s method
model_lm <- function(spp_col){
  m_s <- lm(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
             data = spp_pv, na.action = na.omit)
  }

# confidence estimates and intervals
  model_est <- function(model_x){data.frame(estimate = summary(model_x)$coef[,1],
                                          upper = confint(model_x)[,2],
                                          lower = confint(model_x)[,1]) }

# store parameter estimates
  est_list <- list()
  
# (1) All Andropogoneae ---------------------------------
# model_lm
  spp_col <- spp_pv$All.Andropogoneae
  all_andro_m <- model_lm(spp_col)
  est_list[[1]] <- model_est(all_andro_m)
  
# (2) All C3 --------------------------------------------
# model_lm
  spp_col <- spp_pv$All.C3
  all_c3_m <- model_lm(spp_col)
  est_list[[2]] <- model_est(all_c3_m)
  
# (3) All C4 --------------------------------------------
# model_lm
  spp_col <- spp_pv$All.C4
  all_c4_m <- model_lm(spp_col)
  est_list[[3]] <- model_est(all_c4_m)
  
# (4) All Chloridoideae ---------------------------------
# model_lm
  spp_col <- spp_pv$All.Chloridoideae
  all_chlo_m <- model_lm(spp_col)
  est_list[[4]] <- model_est(all_chlo_m)
  
# (5) All Micrairoideae ---------------------------------
# model_lm
  spp_col <- spp_pv$All.Micrairoideae
  all_micr_m <- model_lm(spp_col)
  est_list[[5]] <- model_est(all_micr_m)
  
# (6) All Paniceae --------------------------------------
# model_lm
  spp_col <- spp_pv$All.Paniceae
  all_pani_m <- model_lm(spp_col)
  est_list[[6]] <- model_est(all_pani_m)
  
# (7) Endemic Andropogoneae ----------------------------
# model_lm
  spp_col <- spp_pv$Endemic.Andropogoneae
  end_andro_m <- model_lm(spp_col)
  est_list[[7]] <- model_est(end_andro_m)
  
# (8) Endemic C3 ---------------------------------------
# model_lm 
  spp_col <- spp_pv$Endemic.C3
  end_c3_m <- model_lm(spp_col)
  est_list[[8]] <- model_est(end_c3_m)
  
# (9) Endemic C4 ---------------------------------------
# model_lm
  spp_col <- spp_pv$Endemic.C4
  end_c4_m <- model_lm(spp_col)
  est_list[[9]] <- model_est(end_c4_m)
  
# (10) Endemic Chloridoideae ----------------------------
# model_lm
  spp_col <- spp_pv$Endemic.Chloridoideae
  end_chlo_m <- model_lm(spp_col)
  est_list[[10]] <- model_est(end_chlo_m)
  
# (11) Endemic Paniceae ---------------------------------
# model_lm 
  spp_col <- spp_pv$Endemic.Paniceae
  end_pani_m <- model_lm(spp_col)
  est_list[[11]] <- model_est(end_pani_m)
  
# (12) Endemic species -----------------------------------
# model_lm
  spp_col <- spp_pv$Endemic.species
  end_spp_m <- model_lm(spp_col)
  est_list[[12]] <- model_est(end_spp_m)
  
# (13) Height all ---------------------------------------
# model_lm
  spp_col <- spp_pv$height.All
  hgt_all_m <- model_lm(spp_col)
  est_list[[13]] <- model_est(hgt_all_m)
  
# (14) Height C4 ----------------------------------------
# model_lm
  spp_col <- spp_pv$height.C4
  hgt_c4_m <- model_lm(spp_col)
  est_list[[14]] <- model_est(hgt_c4_m)
  
# (15) Native Andropogoneae ------------------------------
# model_lm 
  spp_col <- spp_pv$Native.Andropogoneae
  nat_andro_m <- model_lm(spp_col)
  est_list[[15]] <- model_est(nat_andro_m)
  
  
# (16) Native Chloridoideae ----------------------------
# model_lm
  spp_col <- spp_pv$Native.Chloridoideae
  nat_chlo_m <- model_lm(spp_col)
  est_list[[16]] <- model_est(nat_chlo_m)
  
# (17) Native Paniceae ----------------------------------
# model_lm 
  spp_col <- spp_pv$Native.Paniceae
  nat_pani_m <- model_lm(spp_col)
  est_list[[17]] <- model_est(nat_pani_m)
  
# (18) Native species ------------------------------------
# model_lm
  spp_col <- spp_pv$Native.species
  nat_spp_m <- model_lm(spp_col)
  est_list[[18]] <- model_est(nat_spp_m)
  
# save  -------------------------------------------------
  model_list <- list(all_andro_m, all_c3_m,    all_c4_m,  all_chlo_m, all_micr_m,
                     all_pani_m,  end_andro_m, end_c3_m,  end_c4_m,   end_chlo_m, 
                     end_pani_m,  end_spp_m,   hgt_all_m, hgt_c4_m,   nat_andro_m,    
                     nat_chlo_m,  nat_pani_m,  nat_spp_m)
  
    save.image("data/Rdata/linear models.Rdata")
# -------------------------------------------------------
