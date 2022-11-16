
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
# shape file
  oz <- rgdal::readOGR("data/Australia/Australia shapefile.shp")
  plot(oz)
  
# rasters
  current.list <- list.files(path = "results/rasters",
                             pattern = ".grd", full.names = T)
  current.list
  st <- gsub(pattern = "results/rasters/|.grd$", "", current.list)
  st
  c.stack <- log(stack(current.list))
  c.stack <-  calc(c.stack, fun = function(x) {x[x<0] <- 0; return(x)})
  names(c.stack) <- st
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# raw species richness ------------------------------------
## for scale bar  
  j <- vector()
  
  for (i in 1:length(st)){
    
    j[i] <- ceiling(cellStats(exp(c.stack[[i]]), "max", na.rm = T))
    
  }
  j
  leg_lab <- list(c(45,  25, 10, 5,  1), # 45
                  c(55,  30, 10, 5,  1), # 51
                  c(105, 75, 20, 5, 1),   # 102
                  c(55,  30, 10,  1),     # 54
                  c(20,  10, 5,  1),      # 18
                  
                  c(35,  20, 5,  1),      # 35
                  c(130, 80, 40, 10, 1),  # 130
                  c(30,  15, 5,  1),      # 26
                  c(5,   3,  2,  1),  # 5
                  c(50,  30, 10, 5, 1),      # 48
                  
                  c(85,  50,  20, 10, 5),    # 83
                  c(45,  25, 10, 5,  1),    # 42
                  c(50,  30, 10, 5, 1),    # 46
                  c(90,  60,  20, 5, 1),    # 87
                  c(190, 120, 60, 30, 5),    # 190
                  
                  c(160, 100, 40, 20, 5),     # 157
                  c(110, 75, 20, 5, 1),     # 109
                  c(25,  15,  10, 5,  1),     # 23
                  c(25,  15,  10, 5,  1),     # 22
                  c(15,  10,   5,  1),     # 14
  
                  c(15,  10,  5,  1),     # 12
                  c(15,  10,  5,  1),     # 12
                  c(60,  40, 10, 5, 1),     # 57
                  c(50,  30, 10,  5, 1),     # 42
                  c(12,  8,   4,  1))     # 11
  
  names(leg_lab) <- st
  
# scale bar units
  lt <- c(rep("Species\nrichness", 14), 
          rep("Height\n(cm)",         4), 
          rep("Species\nrichness", 7))
  
  names(lt) <- st
  
# maps v10 -------------------------------------------------
# log richness
  obs_v10_l <- function(raster, title, sr_breaks, legend_title){
    
  # scale breaks and labels
    log_sr <- log(sr_breaks)
    
  # spatial points dataframe 
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
    
  # species richness colours
    colr <- rev(brewer.pal(11, "Spectral"))
  # save 
    save_txt <- paste0("results/maps/sub tribes/", title, ".jpeg")  
    
  # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = oz, aes(x = long, y = lat, group = group),
                   fill = "grey60") +
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = oz, colour = "grey1", 
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) + 
      scale_fill_gradientn(colours = colr, 
                           limits = c(min(log_sr), max(log_sr)),            
                           breaks = log_sr, 
                           labels = sr_breaks,
                           space = "Lab",
                           name = legend_title) +
      coord_fixed(ratio = 38/33, xlim = c(112, 155), ylim = c(-45, -7)) +
      theme_map() +
      theme(legend.direction = "vertical",
            legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(18, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            legend.box.spacing = unit(0.01, "cm"),
            plot.title = element_text(size = 20, face = "bold", 
                                      hjust = 0.1, vjust = -0.5)) +
      guides(fill = guide_colorbar(barheight = unit(5, "cm")))
    
    plot(q)
    
    ggsave(save_txt, plot = last_plot(), width = 15, height = 15, 
           units = "cm", dpi = 500, device = "jpeg")
  } # finish

# map requirements v10 -----------------------------------------  
# raster = raster
# title = plot and save title
# sr_breaks = the raw species richness legend labels (i.e. leg_lab)
# sr_max = max species richness
# legend_title = legend units   
  
# maps --------------------------------------------------------  
  for (i in 1:length(names(c.stack))) {
    
    raster <- c.stack[[i]]
    title <- paste(st[i])
    sr_breaks <- leg_lab[[i]]
    legend_title <- lt[[i]]
    
    obs_v10_l(raster, title, sr_breaks, legend_title)
  }

# --------------------------------------------------------------------------
