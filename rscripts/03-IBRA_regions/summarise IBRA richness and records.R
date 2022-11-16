

# library ----------------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(tidyverse)
 
  rm(list = ls())

# data ---------------------------------------------------------------------------------
# australia 
  aus <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/Australia/Australia 100 km.grd")
  
# IBRA regions
  ibra <- readOGR("data/IBRA regions/IBRA shapefile.shp")
  crs(ibra) <- crs(aus)
  
# records
  recs <- read.csv("data/master data/native grass records by IBRA region.csv") %>%
    dplyr::select(species, ibra_code, ibra_name)
  
# Southwest WA --------------------------------------------------
# •	Geraldton Sandplains GES
# •	Yalgoo YAL
# •	Avon Wheatbelt AVW
# •	Swan plain SWA 
# •	Mallee MAL
# •	Jarrah Forests JAF
# •	Warren WAR
# •	Esperance ESP 
# •	Coolgardie COO
# ---------------------------------------------------------------
# records per ibra region
  swwa_recs <- subset(recs, ibra_code %in% c("GES", "YAL", "AVW", "SWA",
                                             "MAL", "JAF", "WAR", "ESP",
                                             "COO")) %>%
               group_by(ibra_name, ibra_code, species) %>%
               summarise(rec_no = n())
  swwa_recs$greater_region <- rep("sw_wa", nrow(swwa_recs))
  swwa_recs <- swwa_recs %>% select(greater_region, ibra_name, ibra_code, species, rec_no)
  head(swwa_recs)
  
# species richness  
  swwa_sr <- swwa_recs %>% group_by(ibra_name, ibra_code) %>%
                           distinct(species, .keep_all = T) %>%
                           summarise(sr = n())
  swwa_sr$greater_region <- rep("sw_wa", nrow(swwa_sr))
  swwa_sr <- swwa_sr %>% select(greater_region, ibra_name, ibra_code, sr)
  head(swwa_sr)
  

# Eremaea --------------------------------------------------
# •	Nullabor NUL
# •	Hampton HAM
# •	Murchison MUR
# •	Carvarvon CAR
  
# •	Gascoyne GAS
# •	Little Sandy Desert LSD
# •	Gibson Desert GIB
# •	GReat Sandy Desert GSD
  
# •	Great Victoria Desert GVD
# •	Central Ranges CER
# •	Pilbara PIL
# •	Tanami TAN
# ---------------------------------------------------------------
# records per ibra region
  erem_recs <- subset(recs, ibra_code %in% c("NUL", "HAM", "MUR", "CAR",
                                             "GAS", "LSD", "GIB", "GSD",
                                             "GVD", "CER", "CER", "PIL",
                                             "TAN")) %>%
    group_by(ibra_name, ibra_code, species) %>%
    summarise(rec_no = n())
  erem_recs$greater_region <- rep("erem", nrow(erem_recs))
  erem_recs <- erem_recs %>% select(greater_region, ibra_name, ibra_code, species, rec_no)
  head(erem_recs)
  
# species richness  
  erem_sr <- erem_recs %>% group_by(ibra_name, ibra_code) %>%
    distinct(species, .keep_all = T) %>%
    summarise(sr = n())
  erem_sr$greater_region <- rep("erem", nrow(erem_sr))
  erem_sr <- erem_sr %>% select(greater_region, ibra_name, ibra_code, sr)
  head(erem_sr)
  
# Northern WA -----------------------------------------------------
# •	Dampierland DAL
# •	Central Kimberley CEK
# •	North Kimberley NOK
# •	Ord Valley Plain OVP
  
# •	Victoria Bonaparte VIB
# ---------------------------------------------------------------
# records per ibra region
  nwa_recs <- subset(recs, ibra_code %in% c("DAL", "CEK", "NOK", "OVP",
                                            "VIB")) %>%
    group_by(ibra_name, ibra_code, species) %>%
    summarise(rec_no = n())
  nwa_recs$greater_region <- rep("nwa", nrow(nwa_recs))
  nwa_recs <- nwa_recs %>% select(greater_region, ibra_name, ibra_code, species, rec_no)
  head(nwa_recs)
  
# species richness  
  nwa_sr <- nwa_recs %>% group_by(ibra_name, ibra_code) %>%
    distinct(species, .keep_all = T) %>%
    summarise(sr = n())
  nwa_sr$greater_region <- rep("nwa", nrow(nwa_sr))
  nwa_sr <- nwa_sr %>% select(greater_region, ibra_name, ibra_code, sr)
  head(nwa_sr)
  
  
  
  
  

# Savanna country, NT ------------------------------------------
# TIW - Tiwi
# PCK - Pine Creek
# DAC - Darwin coastal
# ARP - Arnhem Plateau
  
# DAB - Daly Basin
# CEA - Central Arnhem
# ARC - Arnhem Coast
# GFU - Gulf Fall and Uplands
  
# GUC - Gulf Coast
# STU - Sturt Plateau
# VIB - (same as WA)
# ---------------------------------------------------------------    
# records per ibra region
  sc_nt_recs <- subset(recs, ibra_code %in% c("TIW", "PCK", "DAC", "ARP",
                                             "DAB", "CEA", "ARC", "GFU",
                                             "GUC", "STU", "VIB")) %>%
    group_by(ibra_name, ibra_code, species) %>%
    summarise(rec_no = n())
  sc_nt_recs$greater_region <- rep("sc_nt", nrow(sc_nt_recs))
  sc_nt_recs <- sc_nt_recs %>% select(greater_region, ibra_name, ibra_code, species, rec_no)
  head(sc_nt_recs)
  
# species richness  
  sc_nt_sr <- sc_nt_recs %>% group_by(ibra_name, ibra_code) %>%
    distinct(species, .keep_all = T) %>%
    summarise(sr = n())
  sc_nt_sr$greater_region <- rep("sc_nt", nrow(sc_nt_sr))
  sc_nt_sr <- sc_nt_sr %>% select(greater_region, ibra_name, ibra_code, sr)
  head(sc_nt_sr)
  
# save ---------------------------------------------------------
# SW WA
  write.csv(swwa_recs, "data/IBRA records/SouthWest WA records by IBRA region.csv")
  write.csv(swwa_sr, "data/IBRA records/SouthWest WA species richness by IBRA region.csv")  
   
# Eremaea 
  write.csv(erem_recs, "data/IBRA records/Eremaea records by IBRA region.csv")
  write.csv(erem_sr, "data/IBRA records/Eremaea species richness by IBRA region.csv")  
# Northern WA 
  write.csv(nwa_recs, "data/IBRA records/Northern WA records by IBRA region.csv")
  write.csv(nwa_sr, "data/IBRA records/Northern WA species richness by IBRA region.csv")
  
# Savannah Country NT  
  write.csv(sc_nt_recs, "data/IBRA records/Savannah Country NT records by IBRA region.csv")
  write.csv(sc_nt_sr, "data/IBRA records/Savannah Country NT species richness by IBRA region.csv")
  
# Northern Territory recs & richness --------------------------------------------------
# ARP, DAB, DAC, PCK
  nt_recs <- recs %>% filter(ibra_code == c("ARP", "DAB", "DAC", "PCK"))
  nt_recs_summary <- nt_recs %>% group_by(ibra_code) %>%
                                 summarise(record_no = n())
  
  nt_sr <- nt_recs %>% group_by(ibra_code) %>%
                       distinct(species, .keep_all = T)
  nt_sr_summary <- nt_recs %>% group_by(ibra_code) %>%
                               distinct(species, .keep_all = T) %>%
                               summarise(spp_rich = n())
  
# save
  write.csv(nt_recs, "data/ALA records/NT records raw.csv", row.names = F)
  write.csv(nt_recs_summary, "data/ALA records/NT records summary.csv", row.names = F)
  
  write.csv(nt_sr, "data/ALA records/NT spp rich raw.csv", row.names = F)
  write.csv(nt_sr_summary, "data/ALA records/NT spp rich summary.csv", row.names = F)
# --------------------------------------------------------------------------------------  
  