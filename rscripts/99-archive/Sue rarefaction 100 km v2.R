
# Rarefaction estimation for Sue

###############################################################################
# 100 km
###############################################################################
# will include: 10 - 50 rarefaction x  C3, C4 -- C4 into sub-family maps 

# based on 'rarefaction estimator v6' script

# date created: 11/3
# last updated: 

# library --------------------------------------------------------
  library(dplyr)
  library(data.table)
  library(raster)
  library(ggplot2)
  
  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Data files")

# data for Sue -----------------------------------------------------------
# # all records: no exotic spp. or Triodia genus
#   data <- readRDS("ALA/2019 ALA master data/master grass data.rds") %>%
#           filter(status == "native", genus != "Triodia") %>%
#           select(species, genus, pp, sub.family)
#           arrange(species)
# # # save 
#   write.csv(data, "ALA/2019 ALA master data/master species data.csv", row.names = F)
#   
# # just species
#   data.spp <- data %>%
#               distinct(species, .keep_all = T)
# # save 
#   write.csv(data, "ALA/2019 ALA master data/master native species data.csv", row.names = F)
# --------------------------------------------------------------------------  

# data ----------------------------------------------------
# poa
  dat <- readRDS("ALA/2019 ALA master data/master grass data.rds") %>%
           filter(status == "native", genus != "Triodia") %>%
           arrange(species)

# raster
  raster <- raster("Australia/aus 100-km.grd")
  plot(raster)
 
# 1. data exploration ----------------------------------------------------------------
# 1.1 number of records per cell across Australia
  xy <- cbind(dat$longitude, dat$latitude)
  
# assign raster cell to each point
  dat$cell <- raster::extract(raster, xy)
  
# total records per cell
  n_tot <- rasterize(xy, raster, fun = function(x, ...) length(x))
  plot(log10(n_tot)) 
  # note: cells outside of continental Australia have records in them; we will deal with oceanic cells later
  
# record-coverage check
  ch <- dat %>%
    group_by(cell) %>%
    summarise(n_rec = n())
  
  x <- getValues(n_tot) 
  rv <- data.frame(cell = 1:length(x), n_tot = x)
  ch.rv <- full_join(ch, rv)
  plot(log(n_tot) ~ log(n_rec), data = ch.rv)
  
# 1.2 number of species per cell across Australia
  spp <- as.numeric(factor(dat$species))  
  n_spp <- rasterize(xy, raster, field = spp, fun = function(x, ...) {length(unique(na.omit(x))) })
  plot(n_spp)
  
# species richness check
  ch <- dat %>%
    group_by(cell) %>%
    summarise(n = length(unique(species)))
  
  x <- getValues(n_spp)  
  rv <- data.frame(cell = 1:length(x), n_tot = x)  
  ch.rv <- full_join(ch, rv)  
  plot(log(n) ~ log(n_tot), data = ch.rv)  
  
# record vs species richness
  all.val <- data.frame(n_spp = getValues(n_spp),
                        n_tot = getValues(n_tot))
  
  ggplot(all.val, aes(y = n_spp, x = n_tot)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    scale_x_log10() +
    scale_y_log10() +
    theme_bw()   # not really assymptoting, eh ... (but looks grand)
  
# 2. rarefaction -------------------------------------------------------------
# 2.1 rarefaction set-up -----------------------------------------------------
# records per cell
  n_rec <- table(dat$cell)
  nr <- data.frame(cell = as.numeric(names(n_rec)), n_rec = as.vector(n_rec))
  
# add number of records per cell to the dataframe
  dat <- full_join(dat, nr)
  
# list of all the cells with spp data in them
  cell.list <- as.numeric(levels(factor(dat$cell)))
  
# 2.2 rarefaction function ---------------------------------------------------
# requires as input: sp = a vector of the species names corresponding to all records in a cell
#                    key = a vector of a key that partitions total rarefied richness for a given cell (i.e. pp (C3/C4) or family (A - E)); here we want C3, then the C4 sub-families, then C4 is them added all together
#                    n = subsample size

  rare_sue <- function(sp, key, n) {
    N <- length(sp)           # number of records
    sp_n <- table(sp)         # number of records for each species
    
  # get the key for each species in alphabetical order; in this case 1 = C3, 2 = c4
    a <- unique(cbind(sp, key))
    a <- a[order(a[, 1]), ]
    a_key <- a[, 2]
    
    out <- numeric(length(sp_n)) # vector to store estimate for each species
    
  # for each species, calculate the expected nymber of occurrences from n records
    for(i in 1:length(sp_n)) {
      out[i] <- 1 - exp(lchoose((N - sp_n[i]), n) - lchoose(N, n))  # use lchoose (i.e. log scale) to avoid problems with tabling big numbers
    }
  # output estimated total richness, and richness of key groups; in this case, pp C3 & C4
    return(c(sum(out[a_key == "C3"]),
             sum(out) - sum(out[a_key == "C3"]), # C4
             sum(out[a_key == "Chloridoideae"]),
             sum(out[a_key == "Paniceae"]),
             sum(out[a_key == "Andropogoneae"]),
             sum(out[a_key == "Micrairoideae"]),
             sum(out[a_key == "Aristidoideae"])))
    
  }

# 2.3 run rarefaction function ------------------------------------------------  
#  matrix to store function output: row for each cell, column for total, C3, C4 richness
   n.min <- seq(10, 50, 5)
   out.rare <- matrix(nrow = length(cell.list), ncol = 7)
  
# multi-cutoff (array; 2538, 6, 9)
  a <- array(dim = c(length(raster), ncol(out.rare), length(n.min)))

  for(i in 1:length(n.min)) {

    rec.no <- n.min[i]
    
    for(j in 1:length(cell.list)) {
      cell <- filter(dat, cell == cell.list[j])
      if(cell$n_rec[1] < n.min) out.rare[j] <- NA else {
        spp <- as.character(cell$species)
        key <- as.character(cell$sub.family)          # here alter for tribe or pp
        out.rare[j, ] <- rare_sue(spp, key, rec.no)
      }
     }
    
    m <- matrix(NA, nrow = length(getValues(raster)), ncol = 7)
    m[cell.list, ] <- out.rare

    a[,,i] <- m

  } # fun end
  
# 2.4 save output ------------------------------------------------
# add in column names
  dimnames(a)[[2]] <- c("C3", "C4", "Chloridoideae", "Paniceae", "Andropogoneae", "Micrairoideae", "Aristidoideae")
  dimnames(a)[[3]] <- c("rare.10", "rare.15", "rare.20", "rare.25", "rare.30",
                                   "rare.35", "rare.40", "rare.45", "rare.50")
  poa.df <- as.data.frame(a)  

# remove oceanic cells using template I made earlier (see 'Australia raster' script)
  cell.cat <- read.csv("Australia/aus 100-km.csv", header = T) %>%
    dplyr::select(-prop.cover)
  
# do this for native and exotic peeps separately    
  poa.land <- cbind(cell.cat, poa.df)
  
  poa.terr <- filter(poa.land, cell.cat == "land")
  
# generate list of occupied cells
  cell.list.x <- poa.terr$cell.id
  
# make a matrix with all missing values (to get from 1003 cells to 2538)
  x <- matrix(NA, nrow = length(raster), ncol = 63)
  
  poa.m <- as.matrix(poa.terr)
  poa.mm <- poa.m[, 3:65]
  class(poa.mm) <- "numeric"
  
# add the occupied cells
  x[cell.list.x, ] <- poa.mm
  colnames(x) <- colnames(poa.df)
  
# save data frames
  write.csv(x, "C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Sue/CSV/100 km Rarefied native richnesss 10 to 50 records.csv", row.names = F)
  
# 2.4 save raster objects of each column -------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Sue/Rasters/100 km") 
# 10 rare --------------------------------------------
  c3.rare.10 <- setValues(raster, x[, 1])
  writeRaster(c3.rare.10, "c3.rare.10.grd", overwrite = T)
  c4.rare.10 <- setValues(raster, x[, 2])
  writeRaster(c4.rare.10, "c4.rare.10.grd", overwrite = T)
  chl.rare.10 <- setValues(raster, x[, 3])
  writeRaster(chl.rare.10, "chl.rare.10.grd", overwrite = T)
  pan.rare.10 <- setValues(raster, x[, 4])
  writeRaster(pan.rare.10, "pan.rare.10.grd", overwrite = T)
  and.rare.10 <- setValues(raster, x[, 5])
  writeRaster(and.rare.10, "and.rare.10.grd", overwrite = T)
  mic.rare.10 <- setValues(raster, x[, 6])
  writeRaster(mic.rare.10, "mic.rare.10.grd", overwrite = T)
  ari.rare.10 <- setValues(raster, x[, 7])
  writeRaster(ari.rare.10, "ari.rare.10.grd", overwrite = T)
  
# 15 rare --------------------------------------------
  c3.rare.15 <- setValues(raster, x[, 8])
  writeRaster(c3.rare.15, "c3.rare.15.grd", overwrite = T)
  c4.rare.15 <- setValues(raster, x[, 9])
  writeRaster(c4.rare.15, "c4.rare.15.grd", overwrite = T)
  chl.rare.15 <- setValues(raster, x[, 10])
  writeRaster(chl.rare.15, "chl.rare.15.grd", overwrite = T)
  pan.rare.15 <- setValues(raster, x[, 11])
  writeRaster(pan.rare.15, "pan.rare.15.grd", overwrite = T)
  and.rare.15 <- setValues(raster, x[, 12])
  writeRaster(and.rare.15, "and.rare.15.grd", overwrite = T)
  mic.rare.15 <- setValues(raster, x[, 13])
  writeRaster(mic.rare.15, "mic.rare.15.grd", overwrite = T)
  ari.rare.15 <- setValues(raster, x[, 14])
  writeRaster(ari.rare.15, "ari.rare.15.grd", overwrite = T)
  
# 20 rare --------------------------------------------
  c3.rare.20 <- setValues(raster, x[, 15])
  writeRaster(c3.rare.20, "c3.rare.20.grd", overwrite = T)
  c4.rare.20 <- setValues(raster, x[, 16])
  writeRaster(c4.rare.20, "c4.rare.20.grd", overwrite = T)
  chl.rare.20 <- setValues(raster, x[, 17])
  writeRaster(chl.rare.20, "chl.rare.20.grd", overwrite = T)
  pan.rare.20 <- setValues(raster, x[, 18])
  writeRaster(pan.rare.20, "pan.rare.20.grd", overwrite = T)
  and.rare.20 <- setValues(raster, x[, 19])
  writeRaster(and.rare.20, "and.rare.20.grd", overwrite = T)
  mic.rare.20 <- setValues(raster, x[, 20])
  writeRaster(mic.rare.20, "mic.rare.20.grd", overwrite = T)
  ari.rare.20 <- setValues(raster, x[, 21])
  writeRaster(ari.rare.20, "ari.rare.20.grd", overwrite = T)
  
# 25 rare --------------------------------------------
  c3.rare.25 <- setValues(raster, x[, 22])
  writeRaster(c3.rare.25, "c3.rare.25.grd", overwrite = T)
  c4.rare.25 <- setValues(raster, x[, 23])
  writeRaster(c4.rare.25, "c4.rare.25.grd", overwrite = T)
  chl.rare.25 <- setValues(raster, x[, 24])
  writeRaster(chl.rare.25, "chl.rare.25.grd", overwrite = T)
  pan.rare.25 <- setValues(raster, x[, 25])
  writeRaster(pan.rare.25, "pan.rare.25.grd", overwrite = T)
  and.rare.25 <- setValues(raster, x[, 26])
  writeRaster(and.rare.25, "and.rare.25.grd", overwrite = T)
  mic.rare.25 <- setValues(raster, x[, 27])
  writeRaster(mic.rare.25, "mic.rare.25.grd", overwrite = T)
  ari.rare.25 <- setValues(raster, x[, 28])
  writeRaster(ari.rare.25, "ari.rare.25.grd", overwrite = T)
  
# 30 rare --------------------------------------------
  c3.rare.30 <- setValues(raster, x[, 29])
  writeRaster(c3.rare.30, "c3.rare.30.grd", overwrite = T)
  c4.rare.30 <- setValues(raster, x[, 30])
  writeRaster(c4.rare.30, "c4.rare.30.grd", overwrite = T)
  chl.rare.30 <- setValues(raster, x[, 31])
  writeRaster(chl.rare.30, "chl.rare.30.grd", overwrite = T)
  pan.rare.30 <- setValues(raster, x[, 32])
  writeRaster(pan.rare.30, "pan.rare.30.grd", overwrite = T)
  and.rare.30 <- setValues(raster, x[, 33])
  writeRaster(and.rare.30, "and.rare.30.grd", overwrite = T)
  mic.rare.30 <- setValues(raster, x[, 34])
  writeRaster(mic.rare.30, "mic.rare.30.grd", overwrite = T)
  ari.rare.30 <- setValues(raster, x[, 35])
  writeRaster(ari.rare.30, "ari.rare.30.grd", overwrite = T)
  
# 35 rare --------------------------------------------
  c3.rare.35 <- setValues(raster, x[, 36])
  writeRaster(c3.rare.35, "c3.rare.35.grd", overwrite = T)
  c4.rare.35 <- setValues(raster, x[, 37])
  writeRaster(c4.rare.35, "c4.rare.35.grd", overwrite = T)
  chl.rare.35 <- setValues(raster, x[, 38])
  writeRaster(chl.rare.35, "chl.rare.35.grd", overwrite = T)
  pan.rare.35 <- setValues(raster, x[, 39])
  writeRaster(pan.rare.35, "pan.rare.35.grd", overwrite = T)
  and.rare.35 <- setValues(raster, x[, 40])
  writeRaster(and.rare.35, "and.rare.35.grd", overwrite = T)
  mic.rare.35 <- setValues(raster, x[, 41])
  writeRaster(mic.rare.35, "mic.rare.35.grd", overwrite = T)
  ari.rare.35 <- setValues(raster, x[, 42])
  writeRaster(ari.rare.35, "ari.rare.35.grd", overwrite = T)
  
# 40 rare --------------------------------------------
  c3.rare.40 <- setValues(raster, x[, 43])
  writeRaster(c3.rare.40, "c3.rare.40.grd", overwrite = T)
  c4.rare.40 <- setValues(raster, x[, 44])
  writeRaster(c4.rare.40, "c4.rare.40.grd", overwrite = T)
  chl.rare.40 <- setValues(raster, x[, 45])
  writeRaster(chl.rare.40, "chl.rare.40.grd", overwrite = T)
  pan.rare.40 <- setValues(raster, x[, 46])
  writeRaster(pan.rare.40, "pan.rare.40.grd", overwrite = T)
  and.rare.40 <- setValues(raster, x[, 47])
  writeRaster(and.rare.40, "and.rare.40.grd", overwrite = T)
  mic.rare.40 <- setValues(raster, x[, 48])
  writeRaster(mic.rare.40, "mic.rare.40.grd", overwrite = T)
  ari.rare.40<- setValues(raster, x[, 49])
  writeRaster(ari.rare.40, "ari.rare.40.grd", overwrite = T)
  
# 45 rare --------------------------------------------
  c3.rare.45 <- setValues(raster, x[, 50])
  writeRaster(c3.rare.45, "c3.rare.45.grd", overwrite = T)
  c4.rare.45 <- setValues(raster, x[, 51])
  writeRaster(c4.rare.45, "c4.rare.45.grd", overwrite = T)
  chl.rare.45 <- setValues(raster, x[, 52])
  writeRaster(chl.rare.45, "chl.rare.45.grd", overwrite = T)
  pan.rare.45 <- setValues(raster, x[, 53])
  writeRaster(pan.rare.45, "pan.rare.45.grd", overwrite = T)
  and.rare.45 <- setValues(raster, x[, 54])
  writeRaster(and.rare.45, "and.rare.45.grd", overwrite = T)
  mic.rare.45 <- setValues(raster, x[, 55])
  writeRaster(mic.rare.45, "mic.rare.45.grd", overwrite = T)
  ari.rare.45 <- setValues(raster, x[, 56])
  writeRaster(ari.rare.45, "ari.rare.45.grd", overwrite = T)
  
# 50 rare --------------------------------------------
  c3.rare.50 <- setValues(raster, x[, 57])
  writeRaster(c3.rare.50, "c3.rare.50.grd", overwrite = T)
  c4.rare.50 <- setValues(raster, x[, 58])
  writeRaster(c4.rare.50, "c4.rare.50.grd", overwrite = T)
  chl.rare.50 <- setValues(raster, x[, 59])
  writeRaster(chl.rare.50, "chl.rare.50.grd", overwrite = T)
  pan.rare.50 <- setValues(raster, x[, 60])
  writeRaster(pan.rare.50, "pan.rare.50.grd", overwrite = T)
  and.rare.50 <- setValues(raster, x[, 61])
  writeRaster(and.rare.50, "and.rare.50.grd", overwrite = T)
  mic.rare.50 <- setValues(raster, x[, 62])
  writeRaster(mic.rare.50, "mic.rare.50.grd", overwrite = T)
  ari.rare.50 <- setValues(raster, x[, 63])
  writeRaster(ari.rare.50, "ari.rare.50.grd", overwrite = T)
  
# ---------------------------------------------------------  