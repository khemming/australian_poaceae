

# library -------------------------------------------------------------------------
  library(raster)
  library(rgdal)
  library(iNEXT)
  library(tidyverse)

  rm(list = ls())

# data ----------------------------------------------------------------------------
# australia 
  raster <- raster("data/Australia/Australia 1136.grd")

# poaceae
  dat <- read.csv("data/ALA records/master grass records.csv")
  glimpse(dat)
 
# iNEXT function ------------------------------------------------------------------
# function to do rarefaction using both coverage and size rarefaction
# specify minimum number of records, which is used as the size to rarify to in the size rarefaction (default = 15)
# specifiy coverage to rarify to (default = 0.8)
  cs <- function(dat, min.rec = 15, coverage = 0.8) {
  
# assign each point in the dataframe to raster cell
      xy <- cbind(dat$longitude, dat$latitude)
      dat$cell <- raster::extract(raster, xy)
                                           
# number of records per cell
      nr <- dat %>%
            group_by(cell) %>%
            summarise(n.rec = n()) %>%
            filter(!is.na(cell))
            
      dat <- full_join(dat, nr)
    
# filter by min.rec and extract number of records of each species in each cell
      cr <- dat %>%
            ungroup() %>%
            filter(n.rec >= min.rec) %>%
            mutate(species = factor(species)) %>%
            group_by(species, cell) %>%
            summarise(n = n()) 
            
# get a list of the occupied cell numbers
      cell.list <- as.numeric(as.character(levels(factor(cr$cell))))
      
# store coverage output
      out_cov <- numeric()
# store size output
      out_size <- numeric()
# check for warning
      out_warn <- numeric()
      
# do the rarefaction cell by cell  
        for(i in 1:length(cell.list)) {
          td <- data.frame(spp = cr$n[cr$cell == cell.list[i]])
          td <- td[!is.na(td$spp), ]
      
# coverage rarefaction using iNEXT function
# check for warning
          out_warn[i] <- 0
          temp1 <- tryCatch(estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL), warning = function(w) {out_warn[i] <<- 1})
          
# if there was a warning run again
          if(out_warn[i] == 1) temp1 <- estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL)
          out_cov[i] <- temp1[, 4]
      
# size rarefaction using iNEXT function
          temp2 <- estimateD(td, datatype = "abundance", base = "size", level = min.rec, conf = NULL)
          out_size[i] <- temp2[, 4]
        }
      
# put the rarefaction estimates into the raster
# need to include the missing cell values as well as the occupied cells
      cell_cov <- rep(NA, length(getValues(raster)))
      cell_size <- rep(NA, length(getValues(raster)))
      cell_cov_warn <- rep(NA, length(getValues(raster)))
    
# add the occupied cells
      cell_cov[cell.list] <- out_cov
      cell_size[cell.list] <- out_size
    
# coverage estimates with warning cells set to NA
      cell_cov_warn[cell.list] <- out_warn
      out_cov_warn <- ifelse(out_warn == 1, NA, out_cov)
      cell_cov_warn[cell.list] <- out_cov_warn
    
# generate the raster object for estimated richness  
      rast_cov <- setValues(raster, cell_cov)
      rast_size <- setValues(raster, cell_size)
    
# coverage raster with warning cells set to NA
      rast_cov_warn <- setValues(raster, cell_cov_warn)
      
# number of records per cell
      nrec <- rep(NA, length(getValues(raster)))
      nrec[nr$cell] <- nr$n.rec
      
      nrec[nr$cell] <- nr$n.rec
      
# raw species richness
      spp_per_cell <- as.numeric(factor(dat$species))
      n_spp <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
      m_spp <- mask(n_spp, raster)
      plot(m_spp)
      
# return the values for each cell and the rasters
      return(list(cell_cov, rast_cov, cell_size, rast_size, nrec, cell_cov_warn, rast_cov_warn, m_spp))
      
  }
# ------------------------------------------------------------------------------------
# notes -----------------------------------------------------------------------------
# we produced several arrays of origin x pathway
  
# I think I just want 6 and 7:
# but I do not know (yet) how not to calculate the rest, so I will deal with it :) 
  
# these 8 lists in each as follows: [[1]] df sr calculated by coverage - w warning cells
#                                   [[2]] raster sr calculated by coverage - w warning cells
#                                   [[3]] df sr calculated by size (i.e. -rec rarefaction)               
#                                   [[4]] raster sr calculated by size (i.e. 15-rec rarefaction)    
#                                   [[5]] df number of records per cell
#                                   [[6]] df coverage w warning cells removed
#                                   [[7]] raster coverage w warning cells removed
#                                   [[8]] raw species richness       

# filter to group of interest and run function on each
# iNEXT (1) C3, 
# (2) C4, 
# (3-6) tribes (andro, chlorids, panicoids and micraira), 
# (7) endemic, 
# (8) native, 
# (9- 14) endemic & native: andro., chlorids and panicoids

# steps: (a) run function, (b) check a few plots, (c) save raster. Boom.  
  
# ------------------------------------------------------------------------------------
# outputs
# All Andropogoneae -----------------------------------------------------------------
  x <- dat %>%
    filter(sub_tribe == "Andropogoneae") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/All Andropogoneae.grd", overwrite = T)
  
# how do you get 0 richness with this data??
  ze <- data.frame(getValues(raster))
  ze$recs <- getValues(x.rec)
  ze$rich <- getValues(x.inext[[7]])

# All C3 -----------------------------------------------------------------------------
  x <- dat %>%
         filter(pp == "C3") %>%
         mutate(species = factor(species))
       
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))

# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)

# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   

# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/All C3.grd", overwrite = T)
  
# All C4 -----------------------------------------------------------------------------
# note: removing Eriachnes genus
  x <- dat %>%
    filter(pp == "C4") %>%
    filter(genus != "Eriachne") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/All C4.grd", overwrite = T)

# All Chloridoideae -----------------------------------------------------------------------------
  x <- dat %>%
    filter(sub_tribe == "Chloridoideae") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/All Chloridoideae.grd", overwrite = T)
  
# All Micrairoideae -----------------------------------------------------------------------------
  x <- dat %>%
    filter(sub_tribe == "Micrairoideae") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/All Micrairoideae.grd", overwrite = T)
  
# All Paniceae -----------------------------------------------------------------------------
  x <- dat %>%
    filter(sub_tribe == "Paniceae") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/All Paniceae.grd", overwrite = T)
  
  
# All species -----------------------------------------------------------------------------
  x <- dat %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/All species.grd", overwrite = T)
  
  
# Endemic Andropogoneae -------------------------------------------------------------
  x <- dat %>%
    filter(endemism == "endemic" & sub_tribe == "Andropogoneae") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Endemic Andropogoneae.grd", overwrite = T)
  
# Endemic C3 --------------------------------------------------
  x <- dat %>%
    filter(endemism == "endemic" & pp == "C3") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Endemic C3.grd", overwrite = T)
  
# Endemic C3 Paniceae -------------------------------------------------------------
  Endemic_C3_Paniceae <- c("Ancistrachne",
                           "Calyptochloa",
                           "Cleistochloa",
                           "Neurachne",
                           "Thyridolepis")
  x <- dat %>%
    filter(genus %in% Endemic_C3_Paniceae) %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Endemic C3 Paniceae.grd", overwrite = T)
  
  
# Endemic C4 --------------------------------------------------
  x <- dat %>%
    filter(endemism == "endemic" & pp == "C4") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Endemic C4.grd", overwrite = T)
  
# Endemic Chloridoideae -------------------------------------------------------------
  x <- dat %>%
    filter(endemism == "endemic" & sub_tribe == "Chloridoideae") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Endemic Chloridoideae.grd", overwrite = T)
# Endemic Paniceae -------------------------------------------------------------
  x <- dat %>%
    filter(endemism == "endemic" & sub_tribe == "Paniceae") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Endemic Paniceae.grd", overwrite = T)
  
# Endemic species -------------------------------------------------------------
  x <- dat %>%
    filter(endemism == "endemic") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Endemic species.grd", overwrite = T)
  
# (14) Height C4 
# (15) Height All  
# Native Andropogoneae -------------------------------------------------------------
  x <- dat %>%
    filter(endemism == "native" & sub_tribe == "Andropogoneae") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Native Andropogoneae.grd", overwrite = T)
  
# Native C3 --------------------------------------------------------
  x <- dat %>%
    filter(pp == "C3",
           endemism == "native") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Native C3.grd", overwrite = T)
  
# Native Chloridoideae -------------------------------------------------------------
  x <- dat %>%
    filter(endemism == "native",
           sub_tribe == "Chloridoideae") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Native Chloridoideae.grd", overwrite = T)

# Native C3 Paniceae -------------------------------------------------------------
  Native_C3_Paniceae <- c("Entolasia",
                          "Acroceras",
                          "Cyrtococcum",
                          "Oplismenus",
                          "Ottochloa",
                          "Alloteropsis",
                          "Sacciolepis",
                          "Holcolemma")
  
  x <- dat %>%
    filter(genus %in% Native_C3_Paniceae) %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Native C3 Paniceae.grd", overwrite = T)
  
# Native Paniceae -------------------------------------------------------------
  x <- dat %>%
    filter(endemism == "native" & sub_tribe == "Paniceae") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Native Paniceae.grd", overwrite = T)

# Native species -------------------------------------------------------------
  x <- dat %>%
    filter(endemism == "native") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Native species.grd", overwrite = T)
  
  
# Ancient ------------------------------------------
  x <- dat %>%
    filter(ip == "Ancient") %>%
    mutate(species = factor(species))
  table(x$species) 
# 9 genera, see how we go
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Ancient spp.grd", overwrite = T)
  
# Early -------------------------
  x <- dat %>%
    filter(ip == "Early") %>%
    mutate(species = factor(species))
  table(x$species) 
# 15, based on above's results should get something
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Early spp.grd", overwrite = T)
  
# Mid --------------------------------------
  x <- dat %>%
    filter(ip == "Mid") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Mid spp.grd", overwrite = T)
  
# Recent ------------------------------------------
  x <- dat %>%
    filter(ip == "Recent") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Recent spp.grd", overwrite = T)
  
# combined Ancient, Early, Mid (AEM) ------------------------------
  x <- dat %>%
    filter(ip == "Ancient" | 
             ip == "Early" | 
             ip == "Mid") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/AEM spp.grd", overwrite = T)
  
# C3 combined Ancient, Early, Mid (C3 AEM) ------------------------------
  x <- dat %>%
    filter(ip == "Ancient" | 
             ip == "Early" | 
             ip == "Mid" &
             pp == "C3") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/C3 AEM spp.grd", overwrite = T)
  
# C3 Recent --------------------------------
  x <- dat %>%
    filter(ip == "Recent" &
             pp == "C3") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/C3 Recent spp.grd", overwrite = T)
  
# Homegrown C4s -----------------------------------
  x <- dat %>% # dat for Neurachne spp
    filter(species == "Neurachne munroi" |
             genus == "Paraneurachne" |
             genus == "Eriachne") %>%
    mutate(species = factor(species))
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Homegrown C4 spp.grd", overwrite = T)  
# Early endemic ----------------------------------
  x <- dat %>% 
    filter(ip == "Early" &
             endemism == "endemic") %>%
    mutate(species = factor(species)) 
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Early endemic spp", overwrite = T)
  
# Early native ----------------------------------------
  x <- dat %>% 
    filter(ip == "Early" &
             endemism == "native") %>%
    mutate(species = factor(species)) 
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Early native  spp", overwrite = T)
  
# Mid endemic ---------------------------------------  
  x <- dat %>% 
    filter(ip == "Mid" &
             endemism == "endemic") %>%
    mutate(species = factor(species)) 
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Mid endemic spp", overwrite = T)
  
# Mid native -----------------------------------
  x <- dat %>% 
    filter(ip == "Mid" &
             endemism == "native") %>%
    mutate(species = factor(species)) 
  
  x.inext <- cs(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Mid native spp", overwrite = T)
  
# --------------------------------------------------
  
# iNEXT for genera function ------------------------------------------------------------------
# function to do rarefaction using both coverage and size rarefaction
# specify minimum number of records, which is used as the size to rarify to in the size rarefaction (default = 15)
# specifiy coverage to rarify to (default = 0.8)
  cs_gen <- function(dat, min.rec = 15, coverage = 0.8) {
    
  # assign each point in the dataframe to raster cell
    xy <- cbind(dat$longitude, dat$latitude)
    dat$cell <- raster::extract(raster, xy)
    
  # number of records per cell
    nr <- dat %>%
      group_by(cell) %>%
      summarise(n.rec = n()) %>%
      filter(!is.na(cell))
    
    dat <- full_join(dat, nr)
    
  # filter by min.rec and extract number of records of each genus in each cell
    cr <- dat %>%
      ungroup() %>%
      filter(n.rec >= min.rec) %>%
      mutate(genus = factor(genus)) %>%
      group_by(genus, cell) %>%
      summarise(n = n()) 
    
  # get a list of the occupied cell numbers
    cell.list <- as.numeric(as.character(levels(factor(cr$cell))))
    
  # store coverage output
    out_cov <- numeric()
  # store size output
    out_size <- numeric()
  # check for warning
    out_warn <- numeric()
    
  # do the rarefaction cell by cell  
    for(i in 1:length(cell.list)) {
      td <- data.frame(spp = cr$n[cr$cell == cell.list[i]])
      td <- td[!is.na(td$spp), ]
      
    # coverage rarefaction using iNEXT function
    # check for warning
      out_warn[i] <- 0
      temp1 <- tryCatch(estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL), warning = function(w) {out_warn[i] <<- 1})
      
    # if there was a warning run again
      if(out_warn[i] == 1) temp1 <- estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL)
      out_cov[i] <- temp1[, 4]
      
    # size rarefaction using iNEXT function
      temp2 <- estimateD(td, datatype = "abundance", base = "size", level = min.rec, conf = NULL)
      out_size[i] <- temp2[, 4]
    }
    
  # put the rarefaction estimates into the raster
  # need to include the missing cell values as well as the occupied cells
    cell_cov <- rep(NA, length(getValues(raster)))
    cell_size <- rep(NA, length(getValues(raster)))
    cell_cov_warn <- rep(NA, length(getValues(raster)))
    
  # add the occupied cells
    cell_cov[cell.list] <- out_cov
    cell_size[cell.list] <- out_size
    
  # coverage estimates with warning cells set to NA
    cell_cov_warn[cell.list] <- out_warn
    out_cov_warn <- ifelse(out_warn == 1, NA, out_cov)
    cell_cov_warn[cell.list] <- out_cov_warn
    
  # generate the raster object for estimated richness  
    rast_cov <- setValues(raster, cell_cov)
    rast_size <- setValues(raster, cell_size)
    
  # coverage raster with warning cells set to NA
    rast_cov_warn <- setValues(raster, cell_cov_warn)
    
  # number of records per cell
    nrec <- rep(NA, length(getValues(raster)))
    nrec[nr$cell] <- nr$n.rec
    
    nrec[nr$cell] <- nr$n.rec
    
  # raw genus richness
    spp_per_cell <- as.numeric(factor(dat$genus))
    n_spp <- rasterize(xy, raster, field = spp_per_cell, fun = function(x,...) {length(unique(na.omit(x))) })
    m_spp <- mask(n_spp, raster)
    plot(m_spp)
    
  # return the values for each cell and the rasters
    return(list(cell_cov, rast_cov, cell_size, rast_size, nrec, cell_cov_warn, rast_cov_warn, m_spp))
    
  }

# Ancient ------------------------------------------
  x <- dat %>%
    filter(ip == "Ancient") %>%
    mutate(genus = factor(genus))
  table(x$genus) 
# 9 genera, see how we go
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Ancient gen.grd", overwrite = T)
  
# Early -------------------------
  x <- dat %>%
    filter(ip == "Early") %>%
    mutate(genus = factor(genus))
  table(x$genus) 
# 15, based on above's results should get something
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Early gen.grd", overwrite = T)
  
# Mid --------------------------------------
  x <- dat %>%
    filter(ip == "Mid") %>%
    mutate(genus = factor(genus))
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Mid gen.grd", overwrite = T)
  
# Recent ------------------------------------------
  x <- dat %>%
    filter(ip == "Recent") %>%
    mutate(genus = factor(genus))
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Recent gen.grd", overwrite = T)
  
# combined Ancient, Early, Mid (AEM) ------------------------------
  x <- dat %>%
    filter(ip == "Ancient" | 
             ip == "Early" | 
             ip == "Mid") %>%
    mutate(genus = factor(genus))
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/AEM gen.grd", overwrite = T)
  
# C3 combined Ancient, Early, Mid (C3 AEM) ------------------------------
  x <- dat %>%
    filter(ip == "Ancient" | 
             ip == "Early" | 
             ip == "Mid" &
             pp == "C3") %>%
    mutate(genus = factor(genus))
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/C3 AEM gen.grd", overwrite = T)
  
# C3 Recent --------------------------------
  x <- dat %>%
    filter(ip == "Recent" &
             pp == "C3") %>%
    mutate(genus = factor(genus))
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/C3 Recent gen.grd", overwrite = T)
  
# Homegrown C4s -----------------------------------
  x <- dat %>% # dat2 for Neurachne spp
    filter(species == "Neurachne munroi" |
             genus == "Paraneurachne" |
             genus == "Eriachne") %>%
    mutate(genus = factor(genus)) %>%
    dplyr::select(-species)
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Homegrown C4 gen.grd", overwrite = T)  
# Early endemic ----------------------------------
  x <- dat %>% 
    filter(ip == "Early" &
             endemism == "endemic") %>%
    mutate(genus = factor(genus)) 
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Early endemic gen", overwrite = T)
  
# Early native ----------------------------------------
  x <- dat %>% 
    filter(ip == "Early" &
             endemism == "native") %>%
    mutate(genus = factor(genus)) 
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Early native gen", overwrite = T)
  
# Mid endemic ---------------------------------------  
  x <- dat %>% 
    filter(ip == "Mid" &
             endemism == "endemic") %>%
    mutate(genus = factor(genus)) 
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Mid endemic gen", overwrite = T)
  
# Mid native -----------------------------------
  x <- dat %>% 
    filter(ip == "Mid" &
             endemism == "native") %>%
    mutate(genus = factor(genus)) 
  
  x.inext <- cs_gen(x)  
  par(mfrow = c(2, 2))
  
# records
  x.rec <- setValues(raster, x.inext[[5]])
  plot(x.rec)
  
# raw sr
  plot(x.inext[[8]])
  
# sr with warining cells 
  plot(x.inext[[2]])   
  
# plot sr with warining cells removed
  plot(x.inext[[7]])   
  
# save iNEXT results [[7]]
  writeRaster(x.inext[[7]], "results/rasters/Mid native gen", overwrite = T)
  
# ----------------------------------------------------------------------  
