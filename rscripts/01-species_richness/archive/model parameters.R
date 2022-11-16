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
# model 
  model_type <- read.csv("results/csv/model selection.csv")
  
  spp_pv <- read.csv("results/csv/subtribes and predictor variables.csv")
  head(pv)
  
# run lowest AICc spatial ac-corrected model -------------------------------
# s method
  model_s <- function(spp_col){
    m_s <- gls(spp_col ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
        data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  }
# lm method
  model_lm <- function(spp_col){
    m_s <- lm(spp_col ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
               data = spp_pv, na.action = na.omit)
  }    
# confidence estiumtes and intervals
  model_est <- function(model_x){data.frame(intervals(model_x, 0.95, which = "coef")$coef) }

# (1) All C3 --------------------------------------------
# model_s
  all_c3_m <- model_s(All.C3)
  all_c3_i <- data.frame(intervals(all_c3_m, 0.95, which = "coef")$coef)
  
# (2) All C4 --------------------------------------------
# model_s
  all_c4_m <- gls(All.C4 ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  all_c4_i <- data.frame(intervals(all_c4_m, 0.95, which = "coef")$coef)
  
# (3) All Andropogoneae ---------------------------------
# model_s
  all_andr_m <- gls(All.Andropogoneae ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  all_andr_i <- data.frame(intervals(all_andr_m, 0.95, which = "coef")$coef)
  
# (4) All Chloridoideae ---------------------------------
# model_s
  all_chlo_m <- gls(All.Chloridoideae ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  all_chlo_i <- data.frame(intervals(all_chlo_m, 0.95, which = "coef")$coef)

# (5) All Micrairoideae ---------------------------------
# model_lm
  all_micr_m <- lm(All.Micrairoideae ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, na.action = na.omit)
  all_micr_i <- data.frame(intervals(all_micr_m, 0.95, which = "coef")$coef)
  
# (6) All Paniceae --------------------------------------
# model_s
  all_pani_m <- gls(All.Paniceae ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  all_pani_i <- data.frame(intervals(all_pani_m, 0.95, which = "coef")$coef)
  
# (7) Endemic species
# model_s
  end_m <- gls(Endemic.species ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  end_i <- data.frame(intervals(end_m, 0.95, which = "coef")$coef)
  
# (8) Native species ------------------------------------
## model_s
  nat_m <- gls(Native.species ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  nat_i <- data.frame(intervals(nat_m, 0.95, which = "coef")$coef) 
  
# (9) Native Andropogoneae ------------------------------
# model_lm 
  nat_andro_m <- lm(Native.Andropogoneae ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, na.action = na.omit)
  nat_andro_i <- data.frame(intervals(nat_andro_m, 0.95, which = "coef")$coef)
  
# (10) Endemic Andropogoneae ----------------------------
# model_lm
  nat_andro_m <- lm(Endemic.Andropogoneae ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, na.action = na.omit)
  nat_andro_i <- data.frame(intervals(nat_andro_m, 0.95, which = "coef")$coef)
  
# (11) Native  Chloridoideae ----------------------------
# model_s
  end_chlo_m <- gls(Native.Chloridoideae ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  end_chlo_i <- data.frame(intervals(end_chlo_m, 0.95, which = "coef")$coef)

# (12) Endemic Chloridoideae ----------------------------
# model_s
  end_chlo_m <- gls(Endemic.Chloridoideae ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  end_chlo_i <- data.frame(intervals(end_chlo_m, 0.95, which = "coef")$coef)
  
# (13) Native Paniceae ----------------------------------
# model_lm 
  nat_pani_m <- lm(Native.Paniceae ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, na.action = na.omit)
  nat_pani_i <- data.frame(intervals(nat_pani_m, 0.95, which = "coef")$coef)
  
# (14) Endemic Paniceae ---------------------------------
# model_lm 
  end_pani_m <- lm(Endemic.Paniceae ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, na.action = na.omit)
  end_pani_i <- data.frame(intervals(end_pani_m, 0.95, which = "coef")$coef)
  
# (15) Endemic C3 ---------------------------------------
# model_lm 
  end_c3_m <- lm(Endemic.C3 ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, na.action = na.omit)
  end_c3_i <- data.frame(intervals(end_c3_m, 0.95, which = "coef")$coef)
  
# (16) Endemic C4 ---------------------------------------
# model_s
  end_c4_m <- gls(Endemic.C4 ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  end_c4_i <- data.frame(intervals(end_c4_m, 0.95, which = "coef")$coef)
  
# (17) Height all ---------------------------------------
# model_s
  all_hgt_m <- gls(height ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  all_hgt_i <- data.frame(intervals(all_hgt_m, 0.95, which = "coef")$coef)
  
# (18) Height C4 ----------------------------------------
# model_s
  # all_c3_m <- gls(C4_height ~ clay + th + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
  #   data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  # all_c3_i <- data.frame(intervals(all_c3_m, 0.95, which = "coef")$coef)
  
  
  save.image("data/Rdata/models.R")

# -------------------------------------------------------------------------------------    