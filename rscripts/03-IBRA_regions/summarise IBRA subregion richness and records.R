################################################################################
# richness and records among IBRA subregions
################################################################################

# scope ------------------------------------------------------------------------
# data frame of richness? [#1]
# data frame of records too big -- but could send anyways as attachment [X]
# richness and records for each NT sub region (see email for exact codes)

# library ----------------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(tidyverse)
 
  rm(list = ls())

# data --------------------------------------------------------------------------
# australia 
  aus <- raster("C:/Users/s436862/Dropbox/Poaceae/Data files/Australia/Australia 100 km.grd")
  
# IBRA subregions
  sub <- readOGR("data/IBRA regions/IBRA subregion shapefile.shp")
  crs(sub) <- crs(aus)
  
# records
  recs <- read.csv("data/master data/native grass records by IBRA region.csv") %>%
          drop_na()
  head(recs)

# Arnhem coast --------------------------------------------------------------------
# ARC01
# ARC02
# ARC03
# ARC04
# ARC05
  ar_recs <- subset(recs, sub_code %in% c("ARC01", "ARC02", "ARC03", "ARC04",
                                          "ARC05")) %>%
    group_by(ibra_name, ibra_code, sub_name, sub_code, species) %>%
    summarise(rec_no = n())
  
  write.csv(ar_recs, "data/IBRA records/Arnhem coast records by IBRA sub region.csv")
  
# Central Arnhem -------------------------------------------------------------------
# CEA01
# CEA02
  ca_recs <- subset(recs, sub_code %in% c("CEA01", "CEA02")) %>%
    group_by(ibra_name, ibra_code, sub_name, sub_code, species) %>%
    summarise(rec_no = n())
  
  write.csv(ca_recs, "data/IBRA records/Central Arnhem records by IBRA sub region.csv")
  
# ---------------------------------------------------------------------------------
  
  