#########################################################
# iNEXT rarefaction plots for Sue
#########################################################
# date created: 10/7/19
# last modified: 

# aim -------------------------------------------------------------------
# provide Sue with raster plots of the 14 items produced in the relevant iNEXT estimator script

# library --------------------------------------------------------------
  library(gplots)
  library(RColorBrewer)
  library(raster)
  library(ggmap)
  library(rgdal)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(ggThemeAssist)
  library(gplots)
  library(tidyverse)
  library(forcats)
  library(maps)

  rm(list = ls())

# data -----------------------------------------------------------------
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Sue/rasters")
  
# load all rasters in that folder
  current.list <- list.files(pattern = ".grd")
  names <- gsub(pattern = "\\.grd$", "", current.list)
  c.stack <- stack(current.list)
  names(c.stack) <- names
  rare.rich <- as.data.frame(getValues(c.stack))
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv) 

# 1. iNEXT raster plot function v3 -------------------------------------------
# notes ----------------------------------------------------------------------
# v3 = taken from 'EMAPI figures' script
# ----------------------------------------------------------------------------
# function -- remember legend (leg) requirements
  ras_v3 <- function (title, raster, save, scale)  
    
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
      theme_map() +
      ggtitle(title) +
      oz1 +
      geom_raster(aes(fill = value)) +
      scale_fill_gradientn(colours = colour, 
                           limits = scale,                               
                           space = "Lab",
                           na.value = "transparent",
                           guide = "colourbar",
                           name = "Species\nrichness") + 
      coord_equal() +
      coord_cartesian(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme(legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(1, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            aspect.ratio = 0.88,
            plot.title = element_text(size = 26, face = "bold")
            ) +
      oz2
    print(q)
    
    ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
    
  } # finish function

# ------------------------------------------------------------------------

# plots ------------------------------------------------------------------    
# required: 
#           (1) title
#           (2) raster
#           (3) save location (exact "C:/.../native.C3.jpeg')

  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/iNEXT/Sue/graphs") 
   
# (1) C3
  ras_v3("C3",
         C3, 
         "C3.jpeg", 
         c(1, cellStats(C3, stat = 'max', na.rm = T)))
  
# (2) C4
  ras_v3("C4",
         C4, 
         "C4.jpeg", 
         c(1, cellStats(C4, stat = 'max', na.rm = T))) 
  
# (3) Andropogoneae  
  ras_v3("Andropogoneae",
         Andropogoneae, 
         "Andropogoneae.jpeg", 
         c(1, cellStats(Andropogoneae, stat = 'max', na.rm = T))) 
  
# (4) Chloridoideae 
  ras_v3("Chloridoideae",
         Chloridoideae, 
         "Chloridoideae.jpeg", 
         c(1, cellStats(Chloridoideae, stat = 'max', na.rm = T))) 
  
  
# (5) Micrairoideae
  ras_v3("Micrairoideae",
         Micrairoideae, 
         "Micrairoideae.jpeg", 
         c(1, cellStats(Micrairoideae, stat = 'max', na.rm = T))) 
  
# (6) Paniceae 
  ras_v3("Paniceae",
         Paniceae, 
         "Paniceae.jpeg", 
         c(1, cellStats(Paniceae, stat = 'max', na.rm = T))) 
  
  
# (7) endemic species
  ras_v3("Endemic species",
         endemic, 
         "Endemic species.jpeg", 
         c(1, cellStats(endemic, stat = 'max', na.rm = T))) 
  
# (8) native species
  ras_v3("Native species",
         native, 
         "Native species.jpeg", 
         c(1, cellStats(native, stat = 'max', na.rm = T))) 
  
# (9) native Andropogoneae
  ras_v3("Native Andropogoneae",
         Andropogoneae.native, 
         "Andropogoneae.native.jpeg", 
         c(1, cellStats(Andropogoneae.native, stat = 'max', na.rm = T))) 
  
# (10) endemic Andropogoneae
  ras_v3("Endemic Andropogoneae",
         Andropogoneae.endemic, 
         "Andropogoneae.endemic.jpeg", 
         c(1, cellStats(Andropogoneae.endemic, stat = 'max', na.rm = T))) 
  
# (11) native Chloridoideae
  ras_v3("Native Chloridoideae",
         Chloridoideae, 
         "Chloridoideae.native.jpeg", 
         c(1, cellStats(Chloridoideae, stat = 'max', na.rm = T))) 
  
# (12) endemic Chloridoideae
  ras_v3("Endemic Chloridoideae",
         Chloridoideae.native, 
         "Chloridoideae endemic.jpeg", 
         c(1, cellStats(Chloridoideae, stat = 'max', na.rm = T))) 
  
# (13) native Paniceae
  ras_v3("Native Paniceae",
         Paniceae/native, 
         "Paniceae.native.jpeg", 
         c(1, cellStats(Paniceae.native, stat = 'max', na.rm = T))) 
  
# (14) endemic Paniceae
  ras_v3("Endemic Paniceae",
         Paniceae.endemic, 
         "Paniceae endemic.jpeg", 
         c(1, cellStats(Paniceae.endemic, stat = 'max', na.rm = T))) 
  
# (15) endemic C3
  ras_v3("Endemic C3",
         C3.endemic, 
         "C3 endemic.jpeg", 
         c(1, cellStats(C3.endemic, stat = 'max', na.rm = T))) 
  
# (16) endemic C4
  ras_v3("Endemic C4",
         C4.endemic, 
         "C4 endemic.jpeg", 
         c(1, cellStats(C4.endemic, stat = 'max', na.rm = T))) 
  
  
  
  
  