
# library ------------------------------------------------------
  library(tidyverse)
  library(raster)
  library(rgdal)
  library(iNEXT)
  
  rm(list = ls())

# data ---------------------------------------------------------
# australia 
  raster <- raster("data/Australia/Australia 1136.grd")
  
# poaceae
  dat <- read.csv("data/ALA records/master grass records.csv")
  glimpse(dat)

# required sub tribes
  st_names <- c("Native.species",          "Endemic.species",
                "Native.Neo.C3",           "Endemic.Neo.C3",
                                           "Endemic.Paleo.C3",
                "Native.Paniceae.C4",      "Endemic.Paniceae.C4",
                "Native.Chloridoideae.C4", "Endemic.Chloridoideae.C4",
                "Native.Andropogoneae.C4", "Endemic.Andropogoneae.C4",
                                           "Endemic.Paleo.C4")
# paleo C3 genera
  Paleo_C3 <- c("Anisopogon",
                "Amphibromus",
                "Anthoxanthum",
                "Dichelachne",
                "Echinopogon",
                "Pentapogon",
                "Ancistragrostis",
                "Hookerochloa",
                "Saxipoa",
                "Sylvipoa",
                "Dryopoa",
                "Micraira",
                "Coelachne",
                "Isachne")
# estimate species richness --------------------------------------------------
# see iNEXT script for details
  cs <- function(dat, min.rec = 15, coverage = 0.8) {
    xy <- cbind(dat$longitude, dat$latitude)
    dat$cell <- raster::extract(raster, xy)
    nr <- dat %>%
      group_by(cell) %>%
      summarise(n.rec = n()) %>%
      filter(!is.na(cell))
    dat <- full_join(dat, nr)
    cr <- dat %>%
      ungroup() %>%
      filter(n.rec >= min.rec) %>%
      mutate(species = factor(species)) %>%
      group_by(species, cell) %>%
      summarise(n = n()) 
    cell.list <- as.numeric(as.character(levels(factor(cr$cell))))
    out_cov <- numeric()
    out_size <- numeric()
    out_warn <- numeric()
  for(i in 1:length(cell.list)) {
      td <- data.frame(spp = cr$n[cr$cell == cell.list[i]])
      td <- td[!is.na(td$spp), ]
      out_warn[i] <- 0
      temp1 <- tryCatch(estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL), warning = function(w) {out_warn[i] <<- 1})
    if(out_warn[i] == 1) temp1 <- estimateD(td, datatype = "abundance", base = "coverage", level = coverage, conf = NULL)
      out_cov[i] <- temp1[, 4]
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

# 1. Native.species
  x <- dat %>%
    filter(endemism == "native") %>%
    mutate(species = factor(species))
  x.inext <- cs(x)  
  
  Native.species <- x.inext[[7]]
# 2. Endemic.species
  x <- dat %>%
    filter(endemism == "endemic") %>%
    mutate(species = factor(species))
  x.inext <- cs(x)  
  
  Endemic.species <- x.inext[[7]]

# 3. Native.Neo.C3
  x <- dat %>%
         filter(pp == "C3",
                endemism == "native",
                !genus %in% Paleo_C3) %>%
         mutate(species = factor(species))
  x.inext <- cs(x)  
  
  Native.Neo.C3 <- x.inext[[7]]

# 4. Endemic.Neo.C3
  x <- dat %>%
    filter(pp == "C3",
           endemism == "endemic",
           !genus %in% Paleo_C3) %>%
    mutate(species = factor(species))
  x.inext <- cs(x)  
  
  Endemic.Neo.C3 <- x.inext[[7]]

# 5. Endemic.Paleo.C3
  x <- dat %>%
    filter(endemism == "endemic",
           genus %in% Paleo_C3) %>%
    mutate(species = factor(species))
  x.inext <- cs(x)  
  
  Endemic.Paleo.C3 <- x.inext[[7]]
  
# 6. Native.Paniceae.C4
  x <- dat %>%
    filter(sub_tribe == "Paniceae",
           endemism == "native",
           pp == "C4") %>%
    mutate(species = factor(species))
  x.inext <- cs(x)  
  
  Native.Paniceae.C4 <- x.inext[[7]]
# 7. Endemic.Paniceae.C4
  x <- dat %>%
    filter(sub_tribe == "Paniceae",
           endemism == "endemic",
           pp == "C4") %>%
    mutate(species = factor(species))
  x.inext <- cs(x)
  
  Endemic.Paniceae.C4 <- x.inext[[7]]
  
# 8. Native.Chloridoideae.C4
  x <- dat %>%
    filter(sub_tribe == "Chloridoideae",
           endemism == "native",
           pp == "C4") %>%
    mutate(species = factor(species))
  x.inext <- cs(x)  
  Native.Chloridoideae.C4 <- x.inext[[7]]
# 9. Endemic.Chloridoideae.C4
  x <- dat %>%
    filter(sub_tribe == "Chloridoideae",
           endemism == "endemic",
           pp == "C4") %>%
    mutate(species = factor(species))
  x.inext <- cs(x)  
  Endemic.Chloridoideae.C4 <- x.inext[[7]]
  
# 10. Native.Andropogoneae.C4
  x <- dat %>%
    filter(sub_tribe == "Andropogoneae",
           endemism == "native",
           pp == "C4") %>%
    mutate(species = factor(species))
  x.inext <- cs(x)  
  Native.Andropogoneae.C4 <- x.inext[[7]]
# 11. Endemic.Andropogoneae.C4
  x <- dat %>%
    filter(sub_tribe == "Andropogoneae",
           endemism == "endemic",
           pp == "C4") %>%
    mutate(species = factor(species))
  x.inext <- cs(x)  
  Endemic.Andropogoneae.C4 <- x.inext[[7]]

# 12. Endemic.Paleo.C4
  x <- dat %>%
    filter(genus == "Eriachne",
           pp == "C4") %>%
    mutate(species = factor(species))
  x.inext <- cs(x)  
  Endemic.Paleo.C4 <- x.inext[[7]]

# data frame  
  st_stack <- stack(Native.species,          Endemic.species,
                    Native.Neo.C3,           Endemic.Neo.C3,
                    Endemic.Paleo.C3,
                    Native.Paniceae.C4,      Endemic.Paniceae.C4,
                    Native.Chloridoideae.C4, Endemic.Chloridoideae.C4,
                    Native.Andropogoneae.C4, Endemic.Andropogoneae.C4,
                    Endemic.Paleo.C4)

  st_df <- data.frame(getValues(st_stack))
  names(st_df) <- st_names
  head(st_df)

# summarise
## spp richness values to presence/absence (0/1) and sum
  st_df2 <- st_df %>%
              mutate_all(function(x) if_else(is.na(x), 0, 1))
  
  st_sum <- data.frame(st = st_names, sum = NA)

  
  for (i in 1:length(st_names)) {
    
    y <- sum(st_df2[,i], na.rm = T)
    
    st_sum[i,2] <- y
  
    } 
  st_sum  

# format data frame
  st_sum2 <- data.frame(sub_tribe = c("All grasses", 
                                      "C3 neo", 
                                      "C3 paleo", 
                                      "C4 paniceae", 
                                      "C4 Chloridoideae", 
                                      "C4 Andropogoneae", 
                                      "C4 paleo (Eriachne)"),
                        native = NA,
                        endemic = NA)
# all grasses  
  st_sum2[1, 2] <- st_sum[1, 2]
  st_sum2[1, 3] <- st_sum[2, 2]
# C3 neo  
  st_sum2[2, 2] <- st_sum[3, 2]
  st_sum2[2, 3] <- st_sum[4, 2]
# C3 paleo  
  st_sum2[3, 3] <- st_sum[5, 2]
# Paniceae  
  st_sum2[4, 2] <- st_sum[6, 2]
  st_sum2[4, 3] <- st_sum[7, 2]
# Chlorid  
  st_sum2[5, 2] <- st_sum[8, 2]
  st_sum2[5, 3] <- st_sum[9, 2]
# ANDROPOG  
  st_sum2[6, 2] <- st_sum[10, 2]
  st_sum2[6, 3] <- st_sum[11, 2]
# c4 paleo
  st_sum2[7, 3] <- st_sum[12, 2]

  st_sum2  
  
  write.csv(st_sum2, "results/csv/sub tribe cell occupation.csv", row.names = F)
