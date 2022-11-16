
# library --------------------------------------------------
  library(raster)
  library(tidyverse)
  library(janitor)

  rm(list = ls())

# data -----------------------------------------------------
# records
  dat2 <- read.csv("data/ALA records/master grass records.csv") %>% 
  dplyr::select(species, latitude, longitude, genus, sub_tribe, pp, endemism, hgt) %>%
         mutate(status = if_else(sub_tribe == "Micrairoideae", "Paleo",          # create neo/paleo column for C4s
                                              ifelse(sub_tribe == "C3", "C3", "Neo")))
# Australia
  aus <- raster("data/Australia/Australia 1136.grd")
  
# table 1 ------------------------------------------------------
# genera, species records for each of the four sub tribes
  st_gen <- dat2 %>% 
    group_by(sub_tribe) %>%
    distinct(genus, .keep_all = T) %>%
    summarise(gen = n())
  st_gen
  
  st_spp <- dat2 %>% 
    group_by(sub_tribe, endemism) %>%
    distinct(species, .keep_all = T) %>%
    summarise(spp = n()) %>%
    pivot_wider(names_from = endemism, values_from = spp) %>%
    mutate(total_spp = endemic + native) %>%
    rename(end_spp = endemic, nat_spp = native, sub_tribe = sub_tribe)
  st_spp
  
  st_recs <- dat2 %>% 
    group_by(sub_tribe, endemism) %>%
    summarise(recs = n()) %>%
    pivot_wider(names_from = endemism, values_from = recs) %>%
    mutate(total_rec = endemic + native) %>%
    rename(end_rec = endemic, nat_rec = native, sub_tribe = sub_tribe)
  st_recs
  
# amalgamate to table
# row order
  table1 <- left_join(st_gen, st_spp, by = "sub_tribe") %>%
            left_join(st_recs, by = "sub_tribe")
  table1
  
# save
  write.csv(table1, "results/csv/subtribe records, spp and genera.csv", row.names = F)
  
  
# table 2------------------------------------------------------
# genera, species records for neo, paleo and total C4, and C3 and a grand total  
  stat_gen <- dat2 %>% 
              group_by(status) %>%
              distinct(genus, .keep_all = T) %>%
              summarise(gen = n())
  
  c4_gen <- dat2 %>% 
            group_by(pp) %>%
            distinct(genus, .keep_all = T) %>%
            summarise(gen = n()) %>%
            rename(status = pp)
  gen <- rbind(c4_gen[2, ], stat_gen)  
  gen
  
  stat_spp <- dat2 %>% 
              group_by(status, endemism) %>%
              distinct(species, .keep_all = T) %>%
              summarise(spp = n()) %>%
              pivot_wider(names_from = endemism, values_from = spp) %>% 
              mutate(total_spp = endemic + native) %>%
              rename(end_spp = endemic, nat_spp = native)
              
  c4_spp <- dat2 %>% 
            group_by(pp, endemism) %>%
            distinct(species, .keep_all = T) %>%
            summarise(spp = n()) %>%
            pivot_wider(names_from = endemism, values_from = spp) %>%
            mutate(total_spp = endemic + native) %>%
            rename(end_spp = endemic, nat_spp = native, status = pp)
  spp <- rbind(c4_spp[2, ], stat_spp)  
  spp
  
  stat_recs <- dat2 %>% 
               group_by(status, endemism) %>%
               summarise(recs = n()) %>%
               pivot_wider(names_from = endemism, values_from = recs) %>%
               mutate(total_rec = endemic + native) %>%
               rename(end_rec = endemic, nat_rec = native)
  
  c4_recs <- dat2 %>% 
             group_by(pp, endemism) %>%
             summarise(recs = n()) %>%
             pivot_wider(names_from = endemism, values_from = recs) %>%
             mutate(total_rec = endemic + native) %>%
             rename(end_rec = endemic, nat_rec = native, status = pp)
  recs <- rbind(c4_recs[2, ], stat_recs)
  recs

# amalgamate to table
# row order
  ro <- c("neo", "paleo", "C4", "C3")   
  table2 <- left_join(gen, spp, by = "status") %>%
            left_join(recs, by = "status") %>%
            arrange(ro)
  table2
  
# save
  write.csv(table2, "results/csv/pp records, spp and genus.csv", row.names = F)
  
  
# Supp table #1 ---------------------------------------------------
  # s_tab <- dat2 %>%
  #           select(species, pp, sub_tribe, endemism, status, ip, hgt) %>%
  #           distinct(species, .keep_all = T)
  # head(s_tab)
  # write.csv(s_tab, "results/csv/supplmentary genera and spp list.csv", row.names = F)
# -----------------------------------------------------------------  