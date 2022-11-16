######################################################
# summary stats
######################################################
# for 50 km and 100 km scales
# percent cells occupied
# rarefaction cutoff correlations
# maps for all cut offs x sub-family/pp

# library --------------------------------------------------------------------
  library(RColorBrewer)
  library(raster)
  library(ggmap)
  library(rgdal)
  library(ggplot2)
  library(dplyr)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(ggThemeAssist)
  library(gplots)
  library(tidyr)
  library(forcats)
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Sue")
  
  rm(list = ls())

# 100 km percentage of cells occupied ---------------------------------------------------  
# rarefied richness data
  dat.100km <- read.csv("CSV/100 km Rarefied native richnesss 10 to 50 records.csv", header = T)
  
# cell categories
  aus.cells <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/Data files/Australia/aus 100-km.csv", header = T)
  land.cells <- filter(aus.cells, cell.cat == "land") # 1133
  
# land cells
  dat.cells <- cbind(aus.cells, dat.100km)
  dat.cells.f <- filter(dat.cells, cell.cat == "land")

# nas to zero for percentage estimates
 dat.cells.f[is.na(dat.cells.f)] <- 0
 dat.cells.e <- dat.cells.f[,4:ncol(dat.cells.f)]

# the amount of cells occupied for each column
# test1
  d <- dat.cells.e
  k <- length(dat.cells.e[,1])
  c3.10 <- sum(d$C3.rare.10 > 0)/k*100

  ff <- function(x) (sum(x > 0))/k*100
  
  
  d.p <- apply(d, MARGIN = 2, ff)
  dd.p <- data.frame(as.list(d.p))
  
  write.csv(dd.p, "CSV/100 km percent of cells occupied.csv", row.names = F)
  

# 50 km percentage of cells occupied ---------------------------------------------------  
  rm(list = ls())
  
# rarefied richness data
  dat.50km <- read.csv("CSV/50 km Rarefied native richnesss 10 to 50 records.csv", header = T)
  
# cell categories
  aus.cells <- read.csv("C:/Users/s436862/Dropbox/Rarefaction/Data files/Australia/aus 50-km.csv", header = T)
  land.cells <- filter(aus.cells, cell.cat == "land") # 4290
  
# land cells
  dat.cells <- cbind(aus.cells, dat.50km)
  dat.cells.f <- filter(dat.cells, cell.cat == "land")
  
# nas to zero for percentage estimates
  dat.cells.f[is.na(dat.cells.f)] <- 0
  dat.cells.e <- dat.cells.f[,4:ncol(dat.cells.f)]
  
# the amount of cells occupied for each column
# test1
  d <- dat.cells.e
  k <- length(dat.cells.e[,1])
  c3.10 <- sum(d$C3.rare.10 > 0)/k*100
  
  ff <- function(x) (sum(x > 0))/k*100
  
  
  d.p <- apply(d, MARGIN = 2, ff)
  dd.p <- data.frame(as.list(d.p))
  
  write.csv(dd.p, "CSV/50 km percent of cells occupied.csv", row.names = F)  
  
# ----------------------------------------------------------------------------------  
  
# 100 & 50 km correlation matrices --------------------------------------------------------
  rm(list = ls())
  
# 100 km
  dat.100km <- read.csv("CSV/100 km Rarefied native richnesss 10 to 50 records.csv", header = T)
  cor.100 <- cor(dat.100km, use = "complete.obs")
  write.csv(cor.100, "CSV/100 km rarefied richness correlation matrix.csv", row.names = T)
  
# 50 km
  dat.50km <- read.csv("CSV/50 km Rarefied native richnesss 10 to 50 records.csv", header = T) 
  cor.50 <- cor(dat.50km, use = "complete.obs")
  write.csv(cor.50, "CSV/50 km rarefied richness correlation matrix.csv", row.names = T)
  
  
# --------------------------------------------------------------------------------------  
  
  
# maps ------------------------------------------------------------------------------
# required -----------------------------------------------------------------------------------
# (1) raster
# (2) legend [title] (e.g. record number or species richness)
# (3) file save name (e.g. Graphs/Native SR 50-km.jpeg)  
  
# note: standardised legend, therefore 15- and 50- records will appear similar (rather than having the scale at 50-records for each plot, which would show magnitude decreases, we're instead showing relative differences in patterns)

# plot function ---------------------------------------------------------
  eng_ras <- function (raster, save, title)  
    
  {
  # AUS border + NA fill
    oz1 <- borders("world", region = "Australia", fill = "grey60", bg = "white")
    oz2 <- borders(database = "world", regions = "Australia", colour = "black")
    
  # Colour palette for legend
    colour <- rev(brewer.pal(11, "Spectral"))
  # display.brewer.all() 
  # (for more info)
    
  # Plot
    q <- gplot(raster) + 
      theme_map()+
      oz1 +
      ggtitle(title) +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "Rarefied\nrichness",
                           limits = limit) + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(plot.title = element_text(face = "bold", size = (18)),
            legend.justification = "centre",
            legend.position = "right",
            aspect.ratio = 0.88) +
      oz2
    
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function
  
###########################################################
# 50 km ------------------------------------------------
############################################################
# data ------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Sue/Rasters/50 km")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Sue/Graphs/50 km")
  
# rare 10 ------------------------------------------------------
  limit <- c(0, 10)
  
# c3
  eng_ras(c3.rare.10, "C3 richness to 10 records at 50 km.jpeg", 
                      "C3 richness to 10 records at 50 km")
# c4
  eng_ras(c4.rare.10, "C4 richness to 10 records at 50 km.jpeg", 
                      "C4 richness to 10 records at 50 km")
# Chloridae
  eng_ras(chl.rare.10, "Chloridae richness to 10 records at 50 km.jpeg", 
                             "Chloridae richness to 10 records at 50 km")
# Paniceae
  eng_ras(pan.rare.10, "Paniceae richness to 10 records at 50 km.jpeg", 
                      "Paniceae richness to 10 records at 50 km")
# Andropogoneae
  eng_ras(and.rare.10, "Andropogoneae richness to 10 records at 50 km.jpeg", 
                      "Andropogoneae richness to 10 records at 50 km")
# Micrairodeae
  eng_ras(mic.rare.10, "Micrairodeae richness to 10 records at 50 km.jpeg", 
                      "Micrairodeae richness to 10 records at 50 km")
# Aristoideae
  eng_ras(ari.rare.10, "Aristoideae richness to 10 records at 50 km.jpeg", 
                      "Aristoideae richness to 10 records at 50 km")
  
# rare 15 -------------------------------------------------------  
  limit <- c(0, 15)
  
# c3
  eng_ras(c3.rare.15, "C3 richness to 15 records at 50 km.jpeg", 
          "C3 richness to 15 records at 50 km")
# c4
  eng_ras(c4.rare.15, "C4 richness to 15 records at 50 km.jpeg", 
          "C4 richness to 15 records at 50 km")
# Chloridae
  eng_ras(chl.rare.15, "Chloridae richness to 15 records at 50 km.jpeg", 
          "Chloridae richness to 15 records at 50 km")
# Paniceae
  eng_ras(pan.rare.15, "Paniceae richness to 15 records at 50 km.jpeg", 
          "Paniceae richness to 15 records at 50 km")
# Andropogoneae
  eng_ras(and.rare.15, "Andropogoneae richness to 15 records at 50 km.jpeg", 
          "Andropogoneae richness to 15 records at 50 km")
# Micrairodeae
  eng_ras(mic.rare.15, "Micrairodeae richness to 15 records at 50 km.jpeg", 
          "Micrairodeae richness to 15 records at 50 km")
# Aristoideae
  eng_ras(ari.rare.15, "Aristoideae richness to 15 records at 50 km.jpeg", 
          "Aristoideae richness to 15 records at 50 km")
  
  
# rare 20 -------------------------------------------------------  
  limit <- c(0, 20)
  
# c3
  eng_ras(c3.rare.20, "C3 richness to 20 records at 50 km.jpeg", 
          "C3 richness to 20 records at 50 km")
# c4
  eng_ras(c4.rare.20, "C4 richness to 20 records at 50 km.jpeg", 
          "C4 richness to 20 records at 50 km")
# Chloridae
  eng_ras(chl.rare.20, "Chloridae richness to 20 records at 50 km.jpeg", 
          "Chloridae richness to 20 records at 50 km")
# Paniceae
  eng_ras(pan.rare.20, "Paniceae richness to 20 records at 50 km.jpeg", 
          "Paniceae richness to 20 records at 50 km")
# Andropogoneae
  eng_ras(and.rare.20, "Andropogoneae richness to 20 records at 50 km.jpeg", 
          "Andropogoneae richness to 20 records at 50 km")
# Micrairodeae
  eng_ras(mic.rare.20, "Micrairodeae richness to 20 records at 50 km.jpeg", 
          "Micrairodeae richness to 20 records at 50 km")
# Aristoideae
  eng_ras(ari.rare.20, "Aristoideae richness to 20 records at 50 km.jpeg", 
          "Aristoideae richness to 20 records at 50 km")
  
  
  
# rare 25 -------------------------------------------------------  
  limit <- c(0, 25)
  
# c3
  eng_ras(c3.rare.25, "C3 richness to 25 records at 50 km.jpeg", 
          "C3 richness to 25 records at 50 km")
# c4
  eng_ras(c4.rare.25, "C4 richness to 25 records at 50 km.jpeg", 
          "C4 richness to 25 records at 50 km")
# Chloridae
  eng_ras(chl.rare.25, "Chloridae richness to 25 records at 50 km.jpeg", 
          "Chloridae richness to 25 records at 50 km")
# Paniceae
  eng_ras(pan.rare.25, "Paniceae richness to 25 records at 50 km.jpeg", 
          "Paniceae richness to 25 records at 50 km")
# Andropogoneae
  eng_ras(and.rare.25, "Andropogoneae richness to 25 records at 50 km.jpeg", 
          "Andropogoneae richness to 25 records at 50 km")
# Micrairodeae
  eng_ras(mic.rare.25, "Micrairodeae richness to 25 records at 50 km.jpeg", 
          "Micrairodeae richness to 25 records at 50 km")
# Aristoideae
  eng_ras(ari.rare.25, "Aristoideae richness to 25 records at 50 km.jpeg", 
          "Aristoideae richness to 25 records at 50 km")
  
  
# rare 30 -------------------------------------------------------  
  limit <- c(0, 30)
  
# c3
  eng_ras(c3.rare.30, "C3 richness to 30 records at 50 km.jpeg", 
          "C3 richness to 30 records at 50 km")
# c4
  eng_ras(c4.rare.30, "C4 richness to 30 records at 50 km.jpeg", 
          "C4 richness to 30 records at 50 km")
# Chloridae
  eng_ras(chl.rare.30, "Chloridae richness to 30 records at 50 km.jpeg", 
          "Chloridae richness to 30 records at 50 km")
# Paniceae
  eng_ras(pan.rare.30, "Paniceae richness to 30 records at 50 km.jpeg", 
          "Paniceae richness to 30 records at 50 km")
# Andropogoneae
  eng_ras(and.rare.30, "Andropogoneae richness to 30 records at 50 km.jpeg", 
          "Andropogoneae richness to 30 records at 50 km")
# Micrairodeae
  eng_ras(mic.rare.30, "Micrairodeae richness to 30 records at 50 km.jpeg", 
          "Micrairodeae richness to 30 records at 50 km")
# Aristoideae
  eng_ras(ari.rare.30, "Aristoideae richness to 30 records at 50 km.jpeg", 
          "Aristoideae richness to 30 records at 50 km")
  
  
# rare 35 -------------------------------------------------------  
  limit <- c(0, 35)
  
# c3
  eng_ras(c3.rare.35, "C3 richness to 35 records at 50 km.jpeg", 
          "C3 richness to 35 records at 50 km")
# c4
  eng_ras(c4.rare.35, "C4 richness to 35 records at 50 km.jpeg", 
          "C4 richness to 35 records at 50 km")
# Chloridae
  eng_ras(chl.rare.35, "Chloridae richness to 35 records at 50 km.jpeg", 
          "Chloridae richness to 35 records at 50 km")
# Paniceae
  eng_ras(pan.rare.35, "Paniceae richness to 35 records at 50 km.jpeg", 
          "Paniceae richness to 35 records at 50 km")
# Andropogoneae
  eng_ras(and.rare.35, "Andropogoneae richness to 35 records at 50 km.jpeg", 
          "Andropogoneae richness to 35 records at 50 km")
# Micrairodeae
  eng_ras(mic.rare.35, "Micrairodeae richness to 35 records at 50 km.jpeg", 
          "Micrairodeae richness to 35 records at 50 km")
# Aristoideae
  eng_ras(ari.rare.35, "Aristoideae richness to 35 records at 50 km.jpeg", 
          "Aristoideae richness to 35 records at 50 km")
  
  
# rare 40 -------------------------------------------------------  
  limit <- c(0, 40)
  
# c3
  eng_ras(c3.rare.40, "C3 richness to 40 records at 50 km.jpeg", 
          "C3 richness to 40 records at 50 km")
# c4
  eng_ras(c4.rare.40, "C4 richness to 40 records at 50 km.jpeg", 
          "C4 richness to 40 records at 50 km")
# Chloridae
  eng_ras(chl.rare.40, "Chloridae richness to 40 records at 50 km.jpeg", 
          "Chloridae richness to 40 records at 50 km")
# Paniceae
  eng_ras(pan.rare.40, "Paniceae richness to 40 records at 50 km.jpeg", 
          "Paniceae richness to 40 records at 50 km")
# Andropogoneae
  eng_ras(and.rare.40, "Andropogoneae richness to 40 records at 50 km.jpeg", 
          "Andropogoneae richness to 40 records at 50 km")
# Micrairodeae
  eng_ras(mic.rare.40, "Micrairodeae richness to 40 records at 50 km.jpeg", 
          "Micrairodeae richness to 40 records at 50 km")
# Aristoideae
  eng_ras(ari.rare.40, "Aristoideae richness to 40 records at 50 km.jpeg", 
          "Aristoideae richness to 40 records at 50 km")
  
  
# rare 45 -------------------------------------------------------  
  limit <- c(0, 45)
  
# c3
  eng_ras(c3.rare.45, "C3 richness to 45 records at 50 km.jpeg", 
          "C3 richness to 45 records at 50 km")
# c4
  eng_ras(c4.rare.45, "C4 richness to 45 records at 50 km.jpeg", 
          "C4 richness to 45 records at 50 km")
# Chloridae
  eng_ras(chl.rare.45, "Chloridae richness to 45 records at 50 km.jpeg", 
          "Chloridae richness to 45 records at 50 km")
# Paniceae
  eng_ras(pan.rare.45, "Paniceae richness to 45 records at 50 km.jpeg", 
          "Paniceae richness to 45 records at 50 km")
# Andropogoneae
  eng_ras(and.rare.45, "Andropogoneae richness to 45 records at 50 km.jpeg", 
          "Andropogoneae richness to 45 records at 50 km")
# Micrairodeae
  eng_ras(mic.rare.45, "Micrairodeae richness to 45 records at 50 km.jpeg", 
          "Micrairodeae richness to 45 records at 50 km")
# Aristoideae
  eng_ras(ari.rare.45, "Aristoideae richness to 45 records at 50 km.jpeg", 
          "Aristoideae richness to 45 records at 50 km")
  
  
# rare 50 -------------------------------------------------------  
  limit <- c(0, 50)
  
# c3
  eng_ras(c3.rare.50, "C3 richness to 50 records at 50 km.jpeg", 
          "C3 richness to 50 records at 50 km")
# c4
  eng_ras(c4.rare.50, "C4 richness to 50 records at 50 km.jpeg", 
          "C4 richness to 50 records at 50 km")
# Chloridae
  eng_ras(chl.rare.50, "Chloridae richness to 50 records at 50 km.jpeg", 
          "Chloridae richness to 50 records at 50 km")
# Paniceae
  eng_ras(pan.rare.50, "Paniceae richness to 50 records at 50 km.jpeg", 
          "Paniceae richness to 50 records at 50 km")
# Andropogoneae
  eng_ras(and.rare.50, "Andropogoneae richness to 50 records at 50 km.jpeg", 
          "Andropogoneae richness to 50 records at 50 km")
# Micrairodeae
  eng_ras(mic.rare.50, "Micrairodeae richness to 50 records at 50 km.jpeg", 
          "Micrairodeae richness to 50 records at 50 km")
# Aristoideae
  eng_ras(ari.rare.50, "Aristoideae richness to 50 records at 50 km.jpeg", 
          "Aristoideae richness to 50 records at 50 km")
  
# ------------------------------------------------------------------------
###########################################################
# 100 km ------------------------------------------------
############################################################
# data ------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Sue/Rasters/100 km")
  
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  
  c.stack <- stack(current.list)
  names(c.stack) <- names
  
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Sue/Graphs/100 km")
  
# rare 10 ------------------------------------------------------
  limit <- c(0, 10)
  
# c3
  eng_ras(c3.rare.10, "C3 richness to 10 records at 100 km.jpeg", 
          "C3 richness to 10 records at 100 km")
# c4
  eng_ras(c4.rare.10, "C4 richness to 10 records at 100 km.jpeg", 
          "C4 richness to 10 records at 100 km")
# Chloridae
  eng_ras(chl.rare.10, "Chloridae richness to 10 records at 100 km.jpeg", 
          "Chloridae richness to 10 records at 100 km")
# Paniceae
  eng_ras(pan.rare.10, "Paniceae richness to 10 records at 100 km.jpeg", 
          "Paniceae richness to 10 records at 100 km")
# Andropogoneae
  eng_ras(and.rare.10, "Andropogoneae richness to 10 records at 100 km.jpeg", 
          "Andropogoneae richness to 10 records at 100 km")
# Micrairodeae
  eng_ras(mic.rare.10, "Micrairodeae richness to 10 records at 100 km.jpeg", 
          "Micrairodeae richness to 10 records at 100 km")
# Aristoideae
  eng_ras(ari.rare.10, "Aristoideae richness to 10 records at 100 km.jpeg", 
          "Aristoideae richness to 10 records at 100 km")
  
# rare 15 -------------------------------------------------------  
  limit <- c(0, 15)
  
# c3
  eng_ras(c3.rare.15, "C3 richness to 15 records at 100 km.jpeg", 
          "C3 richness to 15 records at 100 km")
# c4
  eng_ras(c4.rare.15, "C4 richness to 15 records at 100 km.jpeg", 
          "C4 richness to 15 records at 100 km")
# Chloridae
  eng_ras(chl.rare.15, "Chloridae richness to 15 records at 100 km.jpeg", 
          "Chloridae richness to 15 records at 100 km")
# Paniceae
  eng_ras(pan.rare.15, "Paniceae richness to 15 records at 100 km.jpeg", 
          "Paniceae richness to 15 records at 100 km")
# Andropogoneae
  eng_ras(and.rare.15, "Andropogoneae richness to 15 records at 100 km.jpeg", 
          "Andropogoneae richness to 15 records at 100 km")
# Micrairodeae
  eng_ras(mic.rare.15, "Micrairodeae richness to 15 records at 100 km.jpeg", 
          "Micrairodeae richness to 15 records at 100 km")
# Aristoideae
  eng_ras(ari.rare.15, "Aristoideae richness to 15 records at 100 km.jpeg", 
          "Aristoideae richness to 15 records at 100 km")
  
  
# rare 20 -------------------------------------------------------  
  limit <- c(0, 20)
  
# c3
  eng_ras(c3.rare.20, "C3 richness to 20 records at 100 km.jpeg", 
          "C3 richness to 20 records at 100 km")
# c4
  eng_ras(c4.rare.20, "C4 richness to 20 records at 100 km.jpeg", 
          "C4 richness to 20 records at 100 km")
# Chloridae
  eng_ras(chl.rare.20, "Chloridae richness to 20 records at 100 km.jpeg", 
          "Chloridae richness to 20 records at 100 km")
# Paniceae
  eng_ras(pan.rare.20, "Paniceae richness to 20 records at 100 km.jpeg", 
          "Paniceae richness to 20 records at 100 km")
# Andropogoneae
  eng_ras(and.rare.20, "Andropogoneae richness to 20 records at 100 km.jpeg", 
          "Andropogoneae richness to 20 records at 100 km")
# Micrairodeae
  eng_ras(mic.rare.20, "Micrairodeae richness to 20 records at 100 km.jpeg", 
          "Micrairodeae richness to 20 records at 100 km")
# Aristoideae
  eng_ras(ari.rare.20, "Aristoideae richness to 20 records at 100 km.jpeg", 
          "Aristoideae richness to 20 records at 100 km")
  
  
  
# rare 25 -------------------------------------------------------  
  limit <- c(0, 25)
  
# c3
  eng_ras(c3.rare.25, "C3 richness to 25 records at 100 km.jpeg", 
          "C3 richness to 25 records at 100 km")
# c4
  eng_ras(c4.rare.25, "C4 richness to 25 records at 100 km.jpeg", 
          "C4 richness to 25 records at 100 km")
# Chloridae
  eng_ras(chl.rare.25, "Chloridae richness to 25 records at 100 km.jpeg", 
          "Chloridae richness to 25 records at 100 km")
# Paniceae
  eng_ras(pan.rare.25, "Paniceae richness to 25 records at 100 km.jpeg", 
          "Paniceae richness to 25 records at 100 km")
# Andropogoneae
  eng_ras(and.rare.25, "Andropogoneae richness to 25 records at 100 km.jpeg", 
          "Andropogoneae richness to 25 records at 100 km")
# Micrairodeae
  eng_ras(mic.rare.25, "Micrairodeae richness to 25 records at 100 km.jpeg", 
          "Micrairodeae richness to 25 records at 100 km")
# Aristoideae
  eng_ras(ari.rare.25, "Aristoideae richness to 25 records at 100 km.jpeg", 
          "Aristoideae richness to 25 records at 100 km")
  
  
# rare 30 -------------------------------------------------------  
  limit <- c(0, 30)
  
# c3
  eng_ras(c3.rare.30, "C3 richness to 30 records at 100 km.jpeg", 
          "C3 richness to 30 records at 100 km")
# c4
  eng_ras(c4.rare.30, "C4 richness to 30 records at 100 km.jpeg", 
          "C4 richness to 30 records at 100 km")
# Chloridae
  eng_ras(chl.rare.30, "Chloridae richness to 30 records at 100 km.jpeg", 
          "Chloridae richness to 30 records at 100 km")
# Paniceae
  eng_ras(pan.rare.30, "Paniceae richness to 30 records at 100 km.jpeg", 
          "Paniceae richness to 30 records at 100 km")
# Andropogoneae
  eng_ras(and.rare.30, "Andropogoneae richness to 30 records at 100 km.jpeg", 
          "Andropogoneae richness to 30 records at 100 km")
# Micrairodeae
  eng_ras(mic.rare.30, "Micrairodeae richness to 30 records at 100 km.jpeg", 
          "Micrairodeae richness to 30 records at 100 km")
# Aristoideae
  eng_ras(ari.rare.30, "Aristoideae richness to 30 records at 100 km.jpeg", 
          "Aristoideae richness to 30 records at 100 km")
  
  
# rare 35 -------------------------------------------------------  
  limit <- c(0, 35)
  
# c3
  eng_ras(c3.rare.35, "C3 richness to 35 records at 100 km.jpeg", 
          "C3 richness to 35 records at 100 km")
# c4
  eng_ras(c4.rare.35, "C4 richness to 35 records at 100 km.jpeg", 
          "C4 richness to 35 records at 100 km")
# Chloridae
  eng_ras(chl.rare.35, "Chloridae richness to 35 records at 100 km.jpeg", 
          "Chloridae richness to 35 records at 100 km")
# Paniceae
  eng_ras(pan.rare.35, "Paniceae richness to 35 records at 100 km.jpeg", 
          "Paniceae richness to 35 records at 100 km")
# Andropogoneae
  eng_ras(and.rare.35, "Andropogoneae richness to 35 records at 100 km.jpeg", 
          "Andropogoneae richness to 35 records at 100 km")
# Micrairodeae
  eng_ras(mic.rare.35, "Micrairodeae richness to 35 records at 100 km.jpeg", 
          "Micrairodeae richness to 35 records at 100 km")
# Aristoideae
  eng_ras(ari.rare.35, "Aristoideae richness to 35 records at 100 km.jpeg", 
          "Aristoideae richness to 35 records at 100 km")
  
  
# rare 40 -------------------------------------------------------  
  limit <- c(0, 40)
  
# c3
  eng_ras(c3.rare.40, "C3 richness to 40 records at 100 km.jpeg", 
          "C3 richness to 40 records at 100 km")
# c4
  eng_ras(c4.rare.40, "C4 richness to 40 records at 100 km.jpeg", 
          "C4 richness to 40 records at 100 km")
  # Chloridae
  eng_ras(chl.rare.40, "Chloridae richness to 40 records at 100 km.jpeg", 
          "Chloridae richness to 40 records at 100 km")
# Paniceae
  eng_ras(pan.rare.40, "Paniceae richness to 40 records at 100 km.jpeg", 
          "Paniceae richness to 40 records at 100 km")
# Andropogoneae
  eng_ras(and.rare.40, "Andropogoneae richness to 40 records at 100 km.jpeg", 
          "Andropogoneae richness to 40 records at 100 km")
# Micrairodeae
  eng_ras(mic.rare.40, "Micrairodeae richness to 40 records at 100 km.jpeg", 
          "Micrairodeae richness to 40 records at 100 km")
# Aristoideae
  eng_ras(ari.rare.40, "Aristoideae richness to 40 records at 100 km.jpeg", 
          "Aristoideae richness to 40 records at 100 km")
  
  
# rare 45 -------------------------------------------------------  
  limit <- c(0, 45)
  
# c3
  eng_ras(c3.rare.45, "C3 richness to 45 records at 100 km.jpeg", 
          "C3 richness to 45 records at 100 km")
# c4
  eng_ras(c4.rare.45, "C4 richness to 45 records at 100 km.jpeg", 
          "C4 richness to 45 records at 100 km")
# Chloridae
  eng_ras(chl.rare.45, "Chloridae richness to 45 records at 100 km.jpeg", 
          "Chloridae richness to 45 records at 100 km")
# Paniceae
  eng_ras(pan.rare.45, "Paniceae richness to 45 records at 100 km.jpeg", 
          "Paniceae richness to 45 records at 100 km")
# Andropogoneae
  eng_ras(and.rare.45, "Andropogoneae richness to 45 records at 100 km.jpeg", 
          "Andropogoneae richness to 45 records at 100 km")
# Micrairodeae
  eng_ras(mic.rare.45, "Micrairodeae richness to 45 records at 100 km.jpeg", 
          "Micrairodeae richness to 45 records at 100 km")
# Aristoideae
  eng_ras(ari.rare.45, "Aristoideae richness to 45 records at 100 km.jpeg", 
          "Aristoideae richness to 45 records at 100 km")
  
  
# rare 50 -------------------------------------------------------  
  limit <- c(0, 50)
  
# c3
  eng_ras(c3.rare.50, "C3 richness to 50 records at 100 km.jpeg", 
          "C3 richness to 50 records at 100 km")
# c4
  eng_ras(c4.rare.50, "C4 richness to 50 records at 100 km.jpeg", 
          "C4 richness to 50 records at 100 km")
# Chloridae
  eng_ras(chl.rare.50, "Chloridae richness to 50 records at 100 km.jpeg", 
          "Chloridae richness to 50 records at 100 km")
# Paniceae
  eng_ras(pan.rare.50, "Paniceae richness to 50 records at 100 km.jpeg", 
          "Paniceae richness to 50 records at 100 km")
# Andropogoneae
  eng_ras(and.rare.50, "Andropogoneae richness to 50 records at 100 km.jpeg", 
          "Andropogoneae richness to 50 records at 100 km")
# Micrairodeae
  eng_ras(mic.rare.50, "Micrairodeae richness to 50 records at 100 km.jpeg", 
          "Micrairodeae richness to 50 records at 100 km")
# Aristoideae
  eng_ras(ari.rare.50, "Aristoideae richness to 50 records at 100 km.jpeg", 
          "Aristoideae richness to 50 records at 100 km")
  
# ------------------------------------------------------------------------