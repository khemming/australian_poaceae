  
# scope --------------------------------------------------------------------
# check relationships between key different drivers between modelling methods

# All C4 aridity

# library
  library(raster)
  library(tidyverse)

  rm(list = ls())

# data ---------------------------------------------------------------------
# raw richness 
  raw <- read.csv("results/csv/sub tribes raw richness 2538.csv")

# log gls models
  log_sr <- read.csv("results/csv/sub tribe and predictor variables 2538.csv") %>%
          select_at(1:17)
  
# predictor variables
  pv <- read.csv("results/csv/predictor variables 2538.csv")

  
# plot: all c4 aridity
# raw richness  
  sr <- plot(raw$All.C4 ~ pv$arid)
  log <- plot(log_sr$All.C4 ~ pv$arid)
  
  sr_m <- lm(raw$All.C4 ~ pv$arid)
  plot(sr_m$residuals)

# log richness  
  
  log_m <- lm(log_sr$All.C4 ~ pv$arid)
  plot(log_m$residuals)
  

  
  