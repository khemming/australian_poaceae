  
# library --------------------------------------------------
  library(raster)
  library(tidyverse)
  library(janitor)
  
  rm(list = ls())
  
# data -----------------------------------------------------
# grass data
  dat2 <- read.csv("data/ALA records/master grass records.csv")
  
  dat <- dat2 %>%
    dplyr::select(-species)

# Australia
  aus <- raster("data/Australia/Australia 1136.grd")
  
# table 1 ------------------------------------------------------
# genera, species records for immigration period
  st_spp <- dat2 %>% 
    group_by(ip) %>%
    distinct(species, .keep_all = T) %>%
    summarise(spp = n())
  st_spp
  
  st_gen <- dat2 %>% 
    group_by(ip) %>%
    distinct(genus, .keep_all = T) %>%
    summarise(genus = n())
  st_gen
  
  st_recs <- dat2 %>% 
    group_by(ip, endemism) %>%
    summarise(recs = n()) %>%
    pivot_wider(names_from = endemism, values_from = recs) %>%
    mutate(total_rec = endemic + native) %>%
    rename(end_rec = endemic, nat_rec = native, ip = ip)
  st_recs
  
  
  
# amalgamate to table
# row order
  table1 <- left_join(st_gen, st_spp, by = "ip") %>%
    left_join(st_recs, by = "ip")
  table1
  
# save
  write.csv(table1, "results/csv/subtribe records, spp and genera by immigration period.csv", row.names = F)
  
# -------------------------------------------------