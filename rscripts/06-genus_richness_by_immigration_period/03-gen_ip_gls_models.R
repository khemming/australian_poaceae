

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
  
  rm(list = ls())
  
# data ---------------------------------------------------------------------   
# sub tribes
  st_pv <- read.csv("results/supplementary materials/genera/genera pv 1146.csv") %>%
           dplyr::select(-Early, -AEM, -C3_AEM, -Mid, -Recent)
  head(st_pv)
  
  names <- c("Ancient", "C3_recent", "Early_endemic", "Early_native", 
             "Homegrown_C4s", "Mid_endemic", "Mid_native")
  length(names)
  
# identify spatial auto-correlation --------------------------------------------------------------
# store all results for supplementary materials
  moran_ls <- list()
  gls_ls <- list()
  model_ls <- list()
  ci_ls <- list()
  cor_str <- matrix(nrow = length(names))
  
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
  for (i in 1:length(names)){
    
    moran_ls[[i]] <- moran_fun(st_pv[, i], i)
    
  }
  
  moran_ls[[1]] # all spp have spatial-autocorrelation
  names(moran_ls) <- names
  
# Moran's I data frame for saving: 4 x length(names)
  m_mat <- round(matrix(unlist(moran_ls), byrow = T, nrow = length(names)), 4)
  row.names(m_mat) <- names
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
  st_col <- st_pv$Ancient
  e_chl_model <- model_sel_fun(st_col) 
  e_chl_model # works fine
  
# run and choose best fit by AICc
  for (i in 1:length(names)){
    st_col <- st_pv[, i]
    gls_ls[[i]] <- model_sel_fun(st_col)
    cor_str[i] <- gls_ls[[i]]$correlation[1] # best correlation structure
  }
  
  cor_str <- data.frame(cor_str = cor_str)
  
# save all gls model data -----------------------------------------------
# in data frame converted from list
## unlist into this matrix    
  gls_mat <- matrix(nrow = 5, ncol = 18 * 7) # ncol = 18 * no. of periods
  
# unlisted gls is is the wrong structure (5 x 18 * periods; we want 5 * sub tribes x 18)
## divide ncol into leading (l) and trailing (t) sequences of 18 (n)  
  n <- 18
  l <- seq(1, (18 * 7), n)
  tail(l)
  t <- seq(n, (18 * 7), 18)
  tail(t)

# rows its going into 
  r <- seq(5, 35, 5) # 5 * no. of sub tribes
  s <- seq(1, 35, 5) 
  
  gls_mat[,] <- unlist(gls_ls, recursive = T)
  gls_mat2 <- matrix(nrow = length(names) * 5, ncol = 18, byrow = F)
  
  for (i in 1:length(names)){
    
    gls_mat2[s[i]:r[i],] <- gls_mat[,l[i]:t[i]]
    
  }
  
  colnames(gls_mat2) <- colnames(gls_ls[[1]])
  rownames(gls_mat2) <- rep(names, each = 5)
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
  
# Ancient ---------------------------------
# model_g
  st_col <- st_pv$Ancient
  model_ls[[1]] <- model_g(st_col)
  ci_ls[[1]] <- model_est(model_ls[[1]])
  
# C3 recent ---------------------------------
# model_s
  st_col <- st_pv$C3_recent
  model_ls[[2]] <- model_s(st_col)
  ci_ls[[2]] <- model_est(model_ls[[2]])
  
# Early endemic ---------------------------------
# model_s
  st_col <- st_pv$Early_endemic
  model_ls[[3]] <- model_s(st_col)
  ci_ls[[3]] <- model_est(model_ls[[3]])
  
# Early native ---------------------------------
# model_g
  st_col <- st_pv$Early_native
  model_ls[[4]] <- model_g(st_col)
  ci_ls[[4]] <- model_est(model_ls[[4]])
  
# Homegrown C4s ---------------------------------
# model_s
  st_col <- st_pv$Homegrown_C4s
  model_ls[[5]] <- model_s(st_col)
  ci_ls[[5]] <- model_est(model_ls[[5]])
  
# Mid endemic ---------------------------------
# model_e
  st_col <- st_pv$Mid_endemic
  model_ls[[6]] <- model_e(st_col)
  ci_ls[[6]] <- model_est(model_ls[[6]])
  
# Mid native ---------------------------------
# model_s
  st_col <- st_pv$Mid_native
  model_ls[[7]] <- model_s(st_col)
  ci_ls[[7]] <- model_est(model_ls[[7]])
  
  
# save ---------------------------------------
  write.csv(m_mat, "data/supplementary materials/genera Morans I.csv", row.names = T)
  write.csv(gls_mat2, "data/supplementary materials/genera GLS model structures.csv", row.names = T)
  
  save.image("data/supplementary materials/genera gls model data.RData")
# ----------------------------------------------------------------------------


  