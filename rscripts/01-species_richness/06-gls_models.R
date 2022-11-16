

# library ------------------------------------------------------------------
  library(dplyr)
  library(broom)
  library(magrittr)
  library(raster)
  library(MuMIn)
  library(ape)
  library(nlme)  
  library(car)
  library(rgdal)
  library(tidyverse)
  
  rm(list = ls())
  
# data ---------------------------------------------------------------------   
# sub tribes
  st_pv <- read.csv("results/csv/sub tribe and predictor variables 1146.csv")
  head(st_pv)
  
  names <- read.csv("results/csv/subtribe names.csv")
  nrow(names)
  
# identify spatial auto-correlation --------------------------------------------------------------
# store all results for supplementary materials
  moran_ls <- list()
  gls_ls <- list()
  model_ls <- list()
  ci_ls <- list()
  cor_str <- matrix(nrow = nrow(names))
  
# identify spatial autocorrelation function (returns p-value)
  moran_fun <- function(st_col, col_no) {
    xy <- st_pv %>% filter(!is.na(st_col),
                           !is.na(lat)) %>%
      dplyr::select(all_of(col_no), long, lat)
    coords = cbind(xy$long, xy$lat)
    w = fields:::rdist(coords)
    m_i <- Moran.I(x = xy[, 1], w = w, na.rm = T)
    return(m_i)
  }
  
# run 
  for (i in 1:nrow(names)){
    
    moran_ls[[i]] <- moran_fun(st_pv[, i], i)
    
  }
  
  moran_ls[[1]] # all spp have spatial-autocorrelation
  names(moran_ls) <- names
  
# Moran's I data frame for saving: 4 x nrow(names)
  m_mat <- round(matrix(unlist(moran_ls), byrow = T, nrow = nrow(names)), 4)
  row.names(m_mat) <- names$names
  colnames(m_mat) <- c("observed","expected", "sd", "p.value")
  m_mat
  
# model selection --------------------------------------------------------------------------------
## test different methods for modelling spatial autocorrelation, and choose best fit

# model formula
  m_formula <- formula(st_col ~ amt + arid + ts + pwarmq + pcoldq + th + clay + proportion_cover)
  
  model_sel_fun <- function(st_col) {
# model methods to account for spatial autocorrelation
    model_e <- gls(m_formula, 
                   data = st_pv, correlation = corExp(form = ~long + lat, nugget=T) , 
                   na.action = na.omit, method = "ML")
    model_g <- gls(m_formula, 
                   data = st_pv, correlation = corGaus(form = ~long + lat, nugget=T), 
                   na.action = na.omit, method = "ML")
    model_s <- gls(m_formula, 
                   data = st_pv, correlation = corSpher(form = ~long + lat, nugget=T), 
                   na.action = na.omit, method = "ML")
    model_r <- gls(m_formula, 
                   data = st_pv, correlation = corRatio(form = ~long + lat, nugget=T), 
                   na.action = na.omit, method = "ML")
    model_lm <- lm(m_formula, 
                   data = st_pv, na.action = na.omit)
# compare models using AICc
    model_sel <- model.sel(model_e, model_g , model_s, model_r, model_lm)
    return(model_sel)
  }
  
# test
  st_col <- st_pv$Height.All
  e_chl_model <- model_sel_fun(st_col) 
  e_chl_model # works fine
  
# run and choose best fit by AICc
  for (i in 1:nrow(names)){
    st_col <- st_pv[, i]
    gls_ls[[i]] <- model_sel_fun(st_col)
    cor_str[i] <- gls_ls[[i]]$correlation[1] # best correlation structure
  }
  
  cor_str <- data.frame(cor_str = cor_str)
  cor_str
# note -------------------------------------------------------------------------
# 1) Linear model
# 
# 2) model_e 
# 
# 3) model_g 
# 
# 4) model_s 
# 
# 5) model_r 
  
# save all gls model data -----------------------------------------------
# in data frame converted from list
## unlist into this matrix    
  gls_mat <- matrix(nrow = 5, ncol = 17 * 48) # ncol = 17 * no. of sub tribes
  
# unlisted gls is is the wrong structure (5 x 17 * sub tribes; we want 5 * sub tribes x 17)
## divide ncol into leading (l) and trailing (t) sequences of 17 (n)  
  n <- 17
  l <- seq(1, (17 * 48), n)
  tail(l)
  t <- seq(n, (17 * 48), n)
  tail(t)

# rows its going into 
  r <- seq(5, 240, 5) # 5 * no. of sub tribes
  s <- seq(1, 236, 5) # "                   " - 4
  
  gls_mat[,] <- unlist(gls_ls, recursive = T)
  gls_mat2 <- matrix(nrow = nrow(names) * 5, ncol = 17, byrow = F)
  
  for (i in 1:nrow(names)){
    
    gls_mat2[s[i]:r[i],] <- gls_mat[,l[i]:t[i]]
    
  }
  
  colnames(gls_mat2) <- colnames(gls_ls[[1]])
  rownames(gls_mat2) <- rep(names[[1]], each = 5)
# check  
  head(gls_mat2)
  
# run lowest-AIC models  ---------------------------------
## dedicated model functions  
  model_e <- function(st_col){gls(m_formula, 
                 data = st_pv, correlation = corExp(form = ~long + lat, nugget=T) , 
                 na.action = na.omit, method = "ML")}
  model_g <- function(st_col){gls(m_formula, 
                 data = st_pv, correlation = corGaus(form = ~long + lat, nugget=T), 
                 na.action = na.omit, method = "ML")}
  model_s <- function(st_col){gls(m_formula, 
                 data = st_pv, correlation = corSpher(form = ~long + lat, nugget=T), 
                 na.action = na.omit, method = "ML")}
  model_r <- function(st_col){gls(m_formula, 
                 data = st_pv, correlation = corRatio(form = ~long + lat, nugget=T), 
                 na.action = na.omit, method = "ML")}

# re-calculate confidence intervals
  model_est <- function(model){
  data.frame(intervals(model, 0.95, which = "coef")$coef)}
  
# (1) AEM gen ---------------------------------
# model_e
  k <- 1
  st_col <- st_pv$AEM.gen
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# (2) AEM spp --------------------------------------------
# model_e
  k <- 2
  st_col <- st_pv$AEM.spp
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# (3) All Andropogoneae  --------------------------------------------
# model_r
  k <- 3
  st_col <- st_pv$All.Andropogoneae
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# (4) All C3 --------------------------------------------
# model_g
  k <- 4
  st_col <- st_pv$All.C3
  model_ls[[k]] <- model_g(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# (5) All C4 --------------------------------------------
# model_e
  k <- 5
  st_col <- st_pv$All.C4
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# (6) All Chloridoideae ---------------------------------
# model_r
  k <- 6
  st_col <- st_pv$All.Chloridoideae
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# (7) All Micrairoideae ---------------------------------
# model_e
  st_col <- st_pv$All.Micrairoideae
  model_ls[[7]] <- model_e(st_col)
  ci_ls[[7]] <- model_est(model_ls[[7]])
  
# (8) All Paniceae --------------------------------------
# model_e
  st_col <- st_pv$All.Paniceae
  model_ls[[8]] <- model_e(st_col)
  ci_ls[[8]] <- model_est(model_ls[[8]])
  
# (9) All species --------------------------------------
# model_e
  st_col <- st_pv$All.species
  model_ls[[9]] <- model_e(st_col)
  ci_ls[[9]] <- model_est(model_ls[[9]])
  
# (10) Ancient gen --------------------------------------
# model_g
  st_col <- st_pv$Ancient.gen
  model_ls[[10]] <- model_g(st_col)
  ci_ls[[10]] <- model_est(model_ls[[10]])
  
# (11) Ancient spp --------------------------------------
# model_g
  st_col <- st_pv$Ancient.spp
  model_ls[[11]] <- model_g(st_col)
  ci_ls[[11]] <- model_est(model_ls[[11]])
  
# (12) C3 AEM gen --------------------------------------
# model_r
  st_col <- st_pv$AEM.gen
  model_ls[[12]] <- model_r(st_col)
  ci_ls[[12]] <- model_est(model_ls[[12]])
  
# (13) C3 AEM spp --------------------------------------
# model_g
  st_col <- st_pv$AEM.spp
  model_ls[[13]] <- model_g(st_col)
  ci_ls[[13]] <- model_est(model_ls[[13]])
  
# 14         C3 Recent gen
  k <- 14
  st_col <- st_pv$C3.Recent.gen
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 15         C3 Recent spp
  k <- 15
  st_col <- st_pv$C3.Recent.spp
  model_ls[[k]] <- model_g(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 16     Early endemic gen
  k <- 16
  st_col <- st_pv$Early.endemic.gen
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 17     Early endemic spp
  k <- 17
  st_col <- st_pv$Early.endemic.spp
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 18             Early gen
  k <- 18
  st_col <- st_pv$Early.gen
  model_ls[[k]] <- model_g(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 19     Early native  spp
  k <- 19
  st_col <- st_pv$Early.native..spp
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 20      Early native gen
  k <- 20
  st_col <- st_pv$Early.native.gen
  model_ls[[k]] <- model_g(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 21             Early spp
  k <- 21
  st_col <- st_pv$Early.spp
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 22 Endemic Andropogoneae
  k <- 22
  st_col <- st_pv$Endemic.Andropogoneae
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 23   Endemic C3 Paniceae
  k <- 23
  st_col <- st_pv$Endemic.C3.Paniceae
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 24            Endemic C3
  k <- 24
  st_col <- st_pv$Endemic.C3
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 25            Endemic C4
  k <- 25
  st_col <- st_pv$Endemic.C4
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 26 Endemic Chloridoideae
  k <- 26
  st_col <- st_pv$Endemic.Chloridoideae
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 27      Endemic Paniceae
  k <- 27
  st_col <- st_pv$Endemic.Paniceae
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 28       Endemic species
  k <- 28
  st_col <- st_pv$Endemic.species
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 29            Height AEM
  k <- 29
  st_col <- st_pv$Height.AEM
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 30            Height All
  k <- 30
  st_col <- st_pv$Height.All
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 31             Height C4
  k <- 31
  st_col <- st_pv$Height.C4
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
# 32         Height Recent
  k <- 32
  st_col <- st_pv$Height.Recent
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 33      Homegrown C4 gen
  k <- 33
  st_col <- st_pv$Homegrown.C4.gen
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 34      Homegrown C4 spp
  k <- 34
  st_col <- st_pv$Homegrown.C4.spp
  model_ls[[k]] <- model_g(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 35       Mid endemic gen
  k <- 35
  st_col <- st_pv$Mid.endemic.gen
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 36       Mid endemic spp
  k <- 36
  st_col <- st_pv$Mid.endemic.spp
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 37               Mid gen
  k <- 37
  st_col <- st_pv$Mid.gen
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
# 38        Mid native gen
  k <- 38
  st_col <- st_pv$Mid.native.gen
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
# 39        Mid native spp
  k <- 39
  st_col <- st_pv$Mid.native.spp
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
# 40               Mid spp
  k <- 40
  st_col <- st_pv$Mid.spp
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
# 41  Native Andropogoneae
  k <- 41
  st_col <- st_pv$Native.Andropogoneae
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
# 42    Native C3 Paniceae
  k <- 42
  st_col <- st_pv$Native.C3.Paniceae
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
# 43             Native C3
  k <- 43
  st_col <- st_pv$Native.C3
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# 44  Native Chloridoideae
  k <-44 
  st_col <- st_pv$Native.Chloridoideae
  model_ls[[k]] <- model_r(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
# 45       Native Paniceae
  k <- 45
  st_col <- st_pv$Native.Paniceae
  model_ls[[k]] <- model_s(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
# 46        Native species
  k <- 46
  st_col <- st_pv$Native.species
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
# 47            Recent gen
  k <- 47
  st_col <- st_pv$Recent.gen
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
# 48            Recent spp
  k <- 48
  st_col <- st_pv$Recent.spp
  model_ls[[k]] <- model_e(st_col)
  ci_ls[[k]] <- model_est(model_ls[[k]])
  
# save data ---------------------------------------------
  names(model_ls) <- names(names)
  names(ci_ls) <- names(names)
  
  write.csv(m_mat, "results/csv/Morans I.csv", row.names = T)
  write.csv(gls_mat2, "results/csv/GLS model structures.csv", row.names = T)
  
  save.image("data/Rdata/models.RData")
# ----------------------------------------------------------------------------

