
# library -----------------------------------------------------------------  
  library(raster)
  library(dplyr)
  library(naniar)
  
  rm(list = ls())

# data ---------------------------------------------------------------------   
# rasters
  current.list <- list.files(path = "results/rasters",
                             pattern = ".grd", full.names = T)
  names <- gsub(pattern = "results/rasters/|.grd$", "", current.list)
  c.stack <- stack(current.list)
  names
  names(c.stack) <- names
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  st <- as.data.frame(c.stack, na.rm = F)
  glimpse(st)
  
# environmental variables from Poaceae project
  pv <- read.csv("C:/Users/s436862/OneDrive/OneDrive - University of Canberra/Poaceae/Results/csv/predictor variables 2538.csv") %>%
        dplyr::select(lat, long, cell_category, proportion_cover, cell_id, amt, arid, ts, pwarmq, pcoldq, th, clay)
  head(pv)
  pv_1146 <- pv %>% filter(!is.na(clay))
  write.csv(pv_1146, "results/csv/predictor variables 1146.csv", row.names = F)
  
# st-pv data frame --------------------------------------------------------  
# raw ricness data frame
  write.csv(st, "results/csv/sub tribes raw richness 2538.csv", row.names = F)
  
# log and scale species richness but not height
  names(st) # find first to last height cols by names
  st_h <- st %>%
          select(Height.AEM:Height.Recent) # insert them here
  
  stl <- st %>%
         select(-c(Height.AEM:Height.Recent)) %>%
         mutate_all(function(x) log(x)) %>%
         mutate_all(function(x) ifelse(is.infinite(x), 0, x)) %>% # removes inf values 
         bind_cols(st_h)
    
# checks
  hist(stl$All.Andropogoneae) # log units
  hist(stl$Height.All)        # exp units

  stl <- stl %>% select(order(colnames(stl)))
  names(stl)

# bind species and EV data and subset to terrestrial only cells   
  st_pv <- cbind(stl, pv)
  glimpse(st_pv)
  
  write.csv(st_pv, "results/csv/sub tribe and predictor variables 2538.csv", row.names = F)      
  
# subset land only cells (some EVs don't have values where there is very low proportion land cover)
  st_pv_red <- bind_cols(stl, pv) %>%
    filter(!is.na(clay))
  write.csv(st_pv_red, "results/csv/sub tribe and predictor variables 1146.csv", row.names = F)      

# subtribe names
  st_names <- data.frame(names)
  st_names
  write.csv(st_names, "results/csv/subtribe names.csv", row.names = F)

# --------------------------------------------------------------------------------