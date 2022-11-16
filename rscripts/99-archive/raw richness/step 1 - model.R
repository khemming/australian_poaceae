# scope --------------------------------------------------------------------
# compare raw richness with gls model richness
  
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
  
# data ---------------------------------------------------------------------   
# sub tribes and predictor variables
  spp <- read.csv("results/csv/sub tribes raw richness 2538.csv")
  pv <- read.csv("results/csv/predictor variables 2538.csv") 
  spp_pv <- cbind(spp, pv) %>%
            filter(cell_category_all == "land",
                   !is.na(clay))
  
# sub tribe names
  names <- read.csv("results/csv/subtribe names.csv")
  nrow(names)
  
# identify spatial auto-correlation --------------------------------------------------------------
# store results
  model_mat <- matrix(ncol = 3, 
                      nrow = nrow(names))
  rownames(model_mat) <- names$names
  colnames(model_mat) <- c("Moran_p", "model_method", "AICc")
  
# save lowest-AIC model method, mean estimates and CIs
  model_list <- list()
  est_list <- list()
  
  lm_list <- list()
  lm_est <- list()
  
# identify spatial autocorrelation function (returns p-value)
  moran_fun <- function(spp_col, col_no) {
    xy <- spp_pv %>% filter(!is.na(spp_col)) %>%
      dplyr::select(all_of(col_no), long, lat)
    coords = cbind(xy$long, xy$lat)
    w = fields:::rdist(coords)
    m_i <- Moran.I(x = xy[, 1], w = w, na.rm = T)
    return(round(m_i$p.value, 5))
  }
  
# run 
  for (i in 1:nrow(names)){
    
    model_mat[i, 1] <- moran_fun(spp_pv[, i], i)
    
  }
  
  model_mat # all sub tribes have spatial-autocorrelation
  
  
# model selection --------------------------------------------------------------------------------
# test different methods for modelling spatial autocorrelation, and choose best fit
  model_sel_fun <- function(spp_col) {
  # model methods to account for spatial autocorrelation
    model_e <- gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                   data = spp_pv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit, method = "ML")
    model_g <- gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                   data = spp_pv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_s <- gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                   data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_r <- gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                   data = spp_pv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")
    model_lm <- lm(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                   data = spp_pv, na.action = na.omit)
  # compare models using AICc
    model_sel <- model.sel(model_e, model_g , model_s, model_r, model_lm)
    return(model_sel)
  }
  
# test
  spp_col <- spp_pv$Endemic.Chloridoideae
  e_chl_model <- model_sel_fun(spp_col) 
  e_chl_model # works fine
  
# run
  for (i in 1:nrow(names)){
    spp_col <- spp_pv[, i]
    table <- model_sel_fun(spp_col) 
    model_mat[i, 2] <- table$correlation[1] # model method
    model_mat[i, 3] <- table$AICc[1]      # AICc score
  }
  
  model_mat
  
# run best gls model for each sub family and the linear model ----------------------
# single gls model functions   
# models
  model_e <- function(spp_col){gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                                   data = spp_pv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit, method = "ML")}
  model_g <- function(spp_col){gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                                   data = spp_pv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")}
  model_s <- function(spp_col){gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                                   data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")}
  model_r <- function(spp_col){gls(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover, 
                                   data = spp_pv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit, method = "ML")}
  
# lm model function
  lmf <- formula(spp_col ~ pcoldq + pwarmq + amt + ts + arid + th + clay + proportion_cover)
  
# re-calculate confidence intervals
  model_est <- function(model){
    data.frame(intervals(model, 0.95, which = "coef")$coef)}
  
# (1) All Andropogoneae ---------------------------------
# model_r
  spp_col <- spp_pv$All.Andropogoneae
  model_list[[1]] <- model_r(spp_col)
  est_list[[1]] <- model_est(model_list[[1]])
  
  lm_list[[1]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[1]] <- confint(lm_list[[1]])
  
# (2) All C3 --------------------------------------------
# model_e
  spp_col <- spp_pv$All.C3
  model_list[[2]] <- model_e(spp_col)
  est_list[[2]] <- model_est(model_list[[2]])
  
  lm_list[[2]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[2]] <- confint(lm_list[[2]])
  
# (3) All C4 --------------------------------------------
# model_e
  spp_col <- spp_pv$All.C4
  model_list[[3]] <- model_e(spp_col)
  est_list[[3]] <- model_est(model_list[[3]])
  
  lm_list[[3]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[3]] <- confint(lm_list[[3]])
  
# (4) All Chloridoideae ---------------------------------
# model_e
  spp_col <- spp_pv$All.Chloridoideae
  model_list[[4]] <- model_e(spp_col)
  est_list[[4]] <- model_est(model_list[[4]])
  
  lm_list[[4]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[4]] <- confint(lm_list[[4]])
  
# (5) All Micrairoideae ---------------------------------
# model_g
  spp_col <- spp_pv$All.Micrairoideae
  model_list[[5]] <- model_g(spp_col)
  est_list[[5]] <- model_est(model_list[[5]])
  
  lm_list[[5]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[5]] <- confint(lm_list[[5]])
  
# (6) All Paniceae --------------------------------------
# model_e
  spp_col <- spp_pv$All.Paniceae
  model_list[[6]] <- model_e(spp_col)
  est_list[[6]] <- model_est(model_list[[6]])
  
  lm_list[[6]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[6]] <- confint(lm_list[[6]])
  
# (7) All species --------------------------------------
# model_e
  spp_col <- spp_pv$All.species
  model_list[[7]] <- model_e(spp_col)
  est_list[[7]] <- model_est(model_list[[7]])
  
  lm_list[[7]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[7]] <- confint(lm_list[[7]])
  
# (8) Endemic Andropogoneae ----------------------------
# model_e
  spp_col <- spp_pv$Endemic.Andropogoneae
  model_list[[8]] <- model_e(spp_col)
  est_list[[8]] <- model_est(model_list[[8]])
  
  lm_list[[8]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[8]] <- confint(lm_list[[8]])
  
# (9) Endemic C3 ---------------------------------------
# model_e 
  spp_col <- spp_pv$Endemic.C3
  model_list[[9]] <- model_e(spp_col)
  est_list[[9]] <- model_est(model_list[[9]])
  
  lm_list[[9]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[9]] <- confint(lm_list[[9]])
  
# (10) Endemic C4 ---------------------------------------
# model_e
  spp_col <- spp_pv$Endemic.C4
  model_list[[10]] <- model_e(spp_col)
  est_list[[10]] <- model_est(model_list[[10]])
  
  lm_list[[10]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[10]] <- confint(lm_list[[10]])
  
# (11) Endemic Chloridoideae ----------------------------
# model_e
  spp_col <- spp_pv$Endemic.Chloridoideae
  model_list[[11]] <- model_e(spp_col)
  est_list[[11]] <- model_est(model_list[[11]])
  
  lm_list[[11]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[11]] <- confint(lm_list[[11]])
  
# (12) Endemic Paniceae ---------------------------------
# model_r
  spp_col <- spp_pv$Endemic.Paniceae
  model_list[[12]] <- model_r(spp_col)
  est_list[[12]] <- model_est(model_list[[12]])
  
  lm_list[[12]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[12]] <- confint(lm_list[[12]])
  
# (13) Endemic species -----------------------------------
# model_s
  spp_col <- spp_pv$Endemic.species
  model_list[[13]] <- model_s(spp_col)
  est_list[[13]] <- model_est(model_list[[13]])
  
  lm_list[[13]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[13]] <- confint(lm_list[[13]])
  
# (14) Native Andropogoneae ------------------------------
# model_s 
  spp_col <- spp_pv$Native.Andropogoneae
  model_list[[14]] <- model_s(spp_col)
  est_list[[14]] <- model_est(model_list[[14]])
  
  lm_list[[14]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[14]] <- confint(lm_list[[14]])
  
# (15) Native Chloridoideae ----------------------------
# model_s
  spp_col <- spp_pv$Native.Chloridoideae
  model_list[[15]] <- model_s(spp_col)
  est_list[[15]] <- model_est(model_list[[15]])
  
  lm_list[[15]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[15]] <- confint(lm_list[[15]])
  
# (16) Native Paniceae ----------------------------------
# model_e 
  spp_col <- spp_pv$Native.Paniceae
  model_list[[16]] <- model_e(spp_col)
  est_list[[16]] <- model_est(model_list[[16]])
  
  lm_list[[16]] <- lm(formula =  lmf, data = spp_pv)
  lm_est[[16]] <- confint(lm_list[[16]])
  
# (17) Native species ------------------------------------
# model_r
  spp_col <- spp_pv$Native.species
  model_list[[17]] <- model_r(spp_col)
  est_list[[17]] <- model_est(model_list[[17]])
  
  
  lm_list[[17]] <- lm(formula =  lmf, data = spp_pv)
  
  lm_est[[17]] <- confint(lm_list[[17]])
  
# save data ------------------------------------------------------------------
  names(model_list) <- names(names)
  names(est_list) <- names(names)
  
  names(lm_list) <- names(names)
  names(lm_est) <- names(names)
  
  save.image("data/Rdata/raw richness models.RData")
# ----------------------------------------------------------------------------
