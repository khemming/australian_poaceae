
# library ----------------------------------------------------------------------  
  library(raster)
  library(tidyverse)
  library(naniar)
  
  rm(list = ls())

# data -------------------------------------------------------------------------   
# rasters
  current.list <- list.files(path = "results/supplementary materials/species/",
                             pattern = ".grd", full.names = T)
  names <- gsub(pattern = "results/supplementary materials/species/|.grd$", "", current.list)
  c.stack <- stack(current.list)
  names
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  st <- as.data.frame(c.stack, na.rm = F)
  glimpse(st)
  
  write.csv(st, "results/supplementary materials/species/raw species richness 2538.csv", row.names = F)

# environmental variables
  pv <- read.csv("C:/Users/s436862/OneDrive/OneDrive - University of Canberra/Poaceae/Results/csv/predictor variables 2538.csv") %>%
    dplyr::select(lat, long, cell_category, proportion_cover, cell_id, amt, arid, ts, pwarmq,   pcoldq, th, clay)
  
# log and scale species richness
  stl <- st %>%
         mutate_all(function(x) log(x)) %>%
         mutate_all(function(x) ifelse(is.infinite(x), 0, x)) # removes inf values 

# checks
  hist(stl$Early) # log units
  hist(stl$Ancient)        # log units

# bind species and EV data and subset to terrestrial only cells   
  st_pv <- cbind(stl, pv)
  glimpse(st_pv)
  
  write.csv(st_pv, "results/supplementary materials/species/species pv 2538.csv", row.names = F)      
  
# subset land only cells (some EVs don't have values where there is very low proportion land cover)
  st_pv_red <- bind_cols(stl, pv) %>%
    filter(!is.na(clay))
  write.csv(st_pv_red, "results/supplementary materials/species/species pv 1146.csv", row.names = F)      

# --------------------------------------------------------------------------------
  