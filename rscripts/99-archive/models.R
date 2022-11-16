####################################################################
# model various richness, and height 
####################################################################

# library ------------------------------------------------------------------
  library(tidyverse)
  library(broom)
  library(magrittr)
  library(raster)
  library(MuMIn)
  library(ape)
  library(nlme)  
  
  rm(list = ls())
 
# raster layers ----------------------------------------------------------------   
  setwd("C:/Users/s436862/Dropbox/Projects/Sue Bryceson/Results/Rasters")
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  spp <- as.data.frame(c.stack, na.rm = F)
  glimpse(spp)
  setwd("C:/Users/s436862/Dropbox/Projects/Sue Bryceson")

# environmental variables
  pv <- read.csv("Results/predictor variables/predictor variables scaled.csv")
  head(pv)

# bind spp and EV data and subset to terrestrial only cells   
  spp_pv <- cbind(spp, pv) %>%
            filter(cell_category_all == "land") %>%
            dplyr::select(-cell_category_all)
  glimpse(spp_pv)
  
# identify spatial autocrrelation in data -----------------------------------------------
  ac_list <- list()
  
# detecting spatial autocorrelation  
  for (i in 1:length(spp)) {
  
  # columns of interest
    spp_col <- spp_pv[i] # / [1] / [i]
   
  # Moran's I
    xy <- spp_pv %>% filter(!is.na(spp_col)) %>%
      dplyr::select(all_of(i), long, lat)
    head(xy)
    geo <- cbind(xy$long, xy$lat)
    samples.dist <- as.matrix(dist(geo))
    dim(samples.dist)
    samples.dist.inv <- 1/samples.dist
    diag(samples.dist.inv) <- 0
    m_i <- Moran.I(xy[, 1], samples.dist.inv, alternative = "greater", na.rm = TRUE) 
    ac_list[[i]] <- m_i
  } # ac finish
  
  ac_list # all p-values are zero -- spatial autocrrelation everywhere
  
  saveRDS(ac_list, "data/Rdata/Moran's I.RDS")
  
# correct for spatial autocorrelation ------------------------------------------------------
  aic_array <- array(data = NA, dim = c(length(spp), 6, 18))
  coef_array <- array(data = NA, dim = c(length(spp), 10, 3))
  
# test different methods for modelling spatial autocorrelation, and choose best fit
  for (i in 1:length(spp)) {
  
  # function 
    function(i){
  # linear model
    model_lm <- lm(i ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt +  proportion_cover, 
      data = spp_pv) 
  # models accounting for spatial autocorrelation
    model_e <- gls(i ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
      data = spp_pv, correlation = corExp(form = ~long + lat, nugget=T) , na.action = na.omit)
    
    model_g <- gls(i ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
      data = spp_pv, correlation = corGaus(form = ~long + lat, nugget=T), na.action = na.omit)
    
    model_s <- gls(i ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
      data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
    
    model_a <- gls(i ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
      data = spp_pv, correlation = corLin(form = ~long + lat, nugget=T), na.action = na.omit)
    
    model_r <- gls(i ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
      data = spp_pv, correlation = corRatio(form = ~long + lat, nugget=T), na.action = na.omit)
    
  # compare models using AICc
    aic_array <- model.sel(model_lm, model_g, model_s , model_a, model_r)
    return(model_sel)
  }}
  
# run function for model selection for lowest AICc
  n_c3_aic <- model_selection(spp_col)
  n_c3_aic[1] # model-S
  
# attain model S and construct mean estiamtes and CIs
  n_c3_m <- gls(spp_col ~ hii + th + pewc + pcoldq + pwarmq + ts + arid + amt + proportion_cover, 
    data = spp_pv, correlation = corSpher(form = ~long + lat, nugget=T), na.action = na.omit)
  n_c3_i <- data.frame(intervals(n_c3_m, 0.95, which = "coef")$coef)
  
  }
  
  
# older thangs ---------------------------------------------------------------  
  # (1) All C3 
  m1 <- lm(All.C3 ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m1) 
  m1.sum <- tidy(m1)
  m1.ci <- confint(m1)

# (2) All C4  
  m2 <- lm(All.C4 ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m2) 
  m2.sum <- tidy(m2)
  m2.ci <- confint(m2)
  
# (3) All Andropogoneae 
  m3 <- lm(All.Andropogoneae ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m3) 
  m3.sum <- tidy(m3)
  m3.ci <- confint(m3)
  
# (4) All Chloridoideae 
  m4 <- lm(All.Chloridoideae ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m4) 
  m4.sum <- tidy(m4)
  m4.ci <- confint(m4)
  
# (5) All Micrairoideae 
  m5 <- lm(All.Micrairoideae ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m5) 
  m5.sum <- tidy(m5)
  m5.ci <- confint(m5)
  
# (6) All Paniceae 
  m6 <- lm(All.Paniceae ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m6) 
  m6.sum <- tidy(m6)
  m6.ci <- confint(m6)
  
# (7) Endemic species 
  m7 <- lm(Endemic.species ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m7) 
  m7.sum <- tidy(m7)
  m7.ci <- confint(m7)
  
# (8) Native species 
  m8 <- lm(Native.species ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m8) 
  m8.sum <- tidy(m8)
  m8.ci <- confint(m8)
  
# (9) Native Andropogoneae 
  m9 <- lm(Native.Andropogoneae ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m9) 
  m9.sum <- tidy(m9)
  m9.ci <- confint(m9)
  
# (10) Endemic Andropogoneae 
  m10 <- lm(Endemic.Andropogoneae ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m10) 
  m10.sum <- tidy(m10)
  m10.ci <- confint(m10)
  
# (11) Native Chloridoideae
  m11 <- lm(Native.Chloridoideae ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m11) 
  m11.sum <- tidy(m11)
  m11.ci <- confint(m11)
  
# (12) Endemic Chloridoideae 
  m12 <- lm(Endemic.Chloridoideae ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m12) 
  m12.sum <- tidy(m12)
  m12.ci <- confint(m12)
  
# (13) Native Paniceae 
  m13 <- lm(Native.Paniceae ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m13) 
  m13.sum <- tidy(m13)
  m13.ci <- confint(m13)

# (14) Endemic Paniceae
  m14 <- lm(Endemic.Paniceae ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m14) 
  m14.sum <- tidy(m14)
  m14.ci <- confint(m14)

# (15) Endemic C3 
  m15 <- lm(Endemic.C3 ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m15) 
  m15.sum <- tidy(m15)
  m15.ci <- confint(m15)

# (16) Endemic C4 
  m16 <- lm(Endemic.C4 ~ pcoldq + pwarmq + amt + ts + arid + clay + th + prop.cover, data = spp.ev)
  summary(m16) 
  m16.sum <- tidy(m16)
  m16.ci <- confint(m16)
 
# --------------------------------------
# coefficient array ------------------------------------------------------------------
# note we are dropping prop.cover

# dim 1: rows; coefficient names 
  row.names <- vs.evs[3:9]
# dim 2: columns; parameter | mean | lower.ci | upper.ci | plot.names [I'll use later]
  col.names <- c("parameter", "estimate", "lower.ci", "upper.ci", "plot.names")
# dim 3: matrices, based on each subtribe (16 as of 28/10)
  matrix.names <- paste0("m", 1:16)
  
# array    
  coef.ar <- array(data = NA,
                   dim = c(7, 5, 16),
                   dimnames = list(row.names, col.names, matrix.names))
  
  coef.ar[1:7,1,] <- row.names
  
# insert coeffficents & CIs ------------------------------------------
# 2:8 excludes the intercept and prop.cover terms
# 'tibble' makes extracting estimates weird and difficult  
# m1  
  coef.ar[,2,"m1"] <- as.data.frame(m1.sum[2:8,2])[,1]
  coef.ar[,3:4,"m1"] <- m1.ci[2:8,]
  
# m2  
  coef.ar[,2,"m2"] <- as.data.frame(m2.sum[2:8,2])[,1]
  coef.ar[,3:4,"m2"] <- m2.ci[2:8,]
  
# m3  
  coef.ar[,2,"m3"] <- as.data.frame(m3.sum[2:8,2])[,1]
  coef.ar[,3:4,"m3"] <- m3.ci[2:8,]
  
# m4  
  coef.ar[,2,"m4"] <- as.data.frame(m4.sum[2:8,2])[,1]
  coef.ar[,3:4,"m4"] <- m4.ci[2:8,]
  
 # m5  
  coef.ar[,2,"m5"] <- as.data.frame(m5.sum[2:8,2])[,1]
  coef.ar[,3:4,"m5"] <- m5.ci[2:8,]
  
# m6  
  coef.ar[,2,"m6"] <- as.data.frame(m6.sum[2:8,2])[,1]
  coef.ar[,3:4,"m6"] <- m6.ci[2:8,]
  
# m7  
  coef.ar[,2,"m7"] <- as.data.frame(m7.sum[2:8,2])[,1]
  coef.ar[,3:4,"m7"] <- m7.ci[2:8,]
  
# m8  
  coef.ar[,2,"m8"] <- as.data.frame(m8.sum[2:8,2])[,1]
  coef.ar[,3:4,"m8"] <- m8.ci[2:8,]
  
# m9  
  coef.ar[,2,"m9"] <- as.data.frame(m9.sum[2:8,2])[,1]
  coef.ar[,3:4,"m9"] <- m9.ci[2:8,]
  
# m10  
  coef.ar[,2,"m10"] <- as.data.frame(m10.sum[2:8,2])[,1]
  coef.ar[,3:4,"m10"] <- m10.ci[2:8,]
  
# m11  
  coef.ar[,2,"m11"] <- as.data.frame(m11.sum[2:8,2])[,1]
  coef.ar[,3:4,"m11"] <- m11.ci[2:8,]
  
# m12  
  coef.ar[,2,"m12"] <- as.data.frame(m12.sum[2:8,2])[,1]
  coef.ar[,3:4,"m12"] <- m12.ci[2:8,]
  
# m13  
  coef.ar[,2,"m13"] <- as.data.frame(m13.sum[2:8,2])[,1]
  coef.ar[,3:4,"m13"] <- m13.ci[2:8,]
  
# m14  
  coef.ar[,2,"m14"] <- as.data.frame(m14.sum[2:8,2])[,1]
  coef.ar[,3:4,"m14"] <- m14.ci[2:8,]
  
# m15  
  coef.ar[,2,"m15"] <- as.data.frame(m15.sum[2:8,2])[,1]
  coef.ar[,3:4,"m15"] <- m15.ci[2:8,]
  
# m16  
  coef.ar[,2,"m16"] <- as.data.frame(m16.sum[2:8,2])[,1]
  coef.ar[,3:4,"m16"] <- m16.ci[2:8,]
  
# check residuals ------------------------------------------------------
  par(mfrow = c(2, 2))
  plot(m1$residuals)
  plot(m2$residuals)
  plot(m3$residuals)
  plot(m4$residuals)
  
  plot(m5$residuals)
  plot(m6$residuals)
  plot(m7$residuals)
  plot(m8$residuals)
  
  plot(m9$residuals)
  plot(m10$residuals)
  plot(m11$residuals)
  plot(m12$residuals)
  
  plot(m13$residuals)
  plot(m14$residuals)
  plot(m15$residuals)
  plot(m16$residuals)
  
# histogram  
  hist(m1$residuals)
  hist(m2$residuals)
  hist(m3$residuals)
  hist(m4$residuals)
  
  hist(m5$residuals)
  hist(m6$residuals)
  hist(m7$residuals)
  hist(m8$residuals)
  
  hist(m9$residuals)
  hist(m10$residuals)
  hist(m11$residuals)
  hist(m12$residuals)
  
  hist(m13$residuals)
  hist(m14$residuals)
  hist(m15$residuals)
  hist(m16$residuals)

# -----------------------------------------------------
  
# save workspace to use for plotting
  save.image("data/Rdata/model_coefficients.RData") 
  
# -----------------------------------------------------
