
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
  library(dplyr)
  library(forcats)
  library(maps)
  library(viridis)
  
  rm(list = ls())
  
# data -----------------------------------------------------------------
# shape file
  oz <- readOGR("data/Australia/Australia shapefile.shp")
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
  leg_lab <- list(c(14,  10,   5,  1),   # 14
                  c(35,  20, 10, 5,  1),   # 35
                  c(45,  25, 10, 5,  1),   # 45
                  c(50,  30, 10, 5,  1),   # 48
                  c(105,  70, 40, 10, 1),   # 104
    
                  c(55,  30, 15, 5,  1),   # 54
                  c(20,  15,  10,  5,  1),   # 18
                  c(35,  20, 10, 5,  1),    # 35
                  c(130, 90, 50, 20, 10, 1),      # 130
                  c(4,    3,  2,  1),       # 4
                  
                  c(10,   8, 5, 3, 1),    # 10
                  c(11,   8, 5, 3, 1),    # 11
                  c(35,  20, 10, 5,  1),    # 33
                  c(4,    3,  2,  1),    # 4
                  c(17,  13, 8,  5,  1),  # 17
                  
                  c(5, 4,  3,  2,  1),  # 5
                  c(15,  10, 8,  5,  1),  # 16
                  c(6, 4,  3,  2,  1),  # 6
                  c(7, 4,  3,  2,  1),  # 7
                  c(4,  3,  2,  1),  # 4
                  
                  c(18,  15, 10,  5,  1),   # 18
                  c(26, 20, 10, 5,  1),   # 26 
                  c(5, 4,  3,  2,  1),   # 5
                  c(50, 30, 15, 5,  1),    # 47 
                  c(85,  60,  30, 10,  1),   # 83
                  
                  c(45, 30, 15, 5,  1),   # 42 
                  c(50, 30, 15, 5,  1),   # 46
                  c(90,  60,  30, 10,  1),   # 88
                  c(45, 30, 15, 5,  1),   # 41
                  c(50, 30, 15, 5,  1),   # 47
                  
                  c(45, 30, 15, 5,  1),   # 43
                  c(40, 30, 15, 5,  1),   # 36
                  c(3, 2         ,  1),   # 3
                  c(15,  10,  5,  1),   # 15
                  c(60,  40, 20, 5, 1),   # 8
                  
                  c(30,  20, 10, 5, 1),   # 28
                  
                  c(7, 5, 3, 2,  1),   # 7
                  c(5, 4, 3, 2,  1),   # 5
                  c(10, 8, 5,  1),   # 10
                  c(30, 15, 10, 5,  1),   # 29
                  c(25, 15, 10, 5,  1),   # 22
                  
                  c(12,  10,   5,  1),   # 12
                  c(13,  10,  5,  1),   # 13
                  c(12,  10,  5,  1),   # 12
                  c(12,  10,  5,  1),   # 12
                  c(55,  40, 20, 5, 1),   # 55
                  
                  c(35, 20, 10,  5, 1),   # 33
                  c(100, 75, 40, 10, 1))   # 95
  
  names(leg_lab) <- st
  
# scale bar units
  lt <- c(rep("Genus\nrichness", 1),    #1
          rep("Species\nrichness", 8),  #9
          rep("Genus\nrichness", 1),    #10
          rep("Species\nrichness", 1),  #11
          rep("Genus\nrichness", 1),    #12
          rep("Species\nrichness", 1),  #13
          rep("Genus\nrichness", 1),    #14
          rep("Species\nrichness", 1),  #15
          rep("Genus\nrichness", 1),    #16
          rep("Species\nrichness", 1),  #17
          rep("Genus\nrichness", 1),    #18
          rep("Species\nrichness", 1),  #19
          rep("Genus\nrichness", 1),    #20
          rep("species\nrichness", 8),  #21-28
          rep("Height\n(cm)",         4), #29-32
          rep("Genus\nrichness", 1),    #33
          rep("Species\nrichness", 1),  #34
          rep("Genus\nrichness", 1),   #35
          rep("Species\nrichness", 1), #36
          rep("Genus\nrichness", 2),   #37-38
          rep("Species\nrichness", 8), #39-46
          rep("Genus\nrichness", 1),  #47
          rep("Species\nrichness", 1)) #48
  
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
  for (i in 1:length(names(c.stack))) {
    
    raster <- c.stack[[i]]
    title <- paste(st[i])
    sr_breaks <- leg_lab[[i]]
    legend_title <- lt[[i]]
    
    obs_v10_l(raster, title, sr_breaks, legend_title)
  }

# height maps v11 -------------------------------------------------
# using raw richness
  obs_v11 <- function(raster, title, sr_breaks, legend_title){
    
  # spatial points dataframe 
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
    
  # species richness colours
    #colr <- viridis(11, "magma", rev = TRUE)
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
      scale_fill_viridis(option = "magma", 
                         direction = 1,
                           limits = c(min(sr_breaks), max(sr_breaks)),            
                           breaks = sr_breaks, 
                           labels = sr_breaks,
                           space = "Lab",
                           name = legend_title) +
      #scale_fill_viridis(option="magma") +
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
  leg_lab2 <- list(c(1),   # 45
                   c(1),   # 51
                   c(1),    # 104
                   c(1),      # 54
                   c(1),       # 18
                   
                   c(1),    # 35
                   c(1),# 130
                   c(1),    # 26
                   c(1),    # 5
                   c(1),  # 48
                   
                   c(1),  # 83
                   c(1),  # 42
                   c(1),  # 46
                   c(1),  # 90
                   c(1),  # 90
                   
                   c(1),  # 90
                   c(1),  # 90
                   c(1),  # 90
                   c(1),  # 90
                   c(1),  # 90
                   
                   c(1),  # 90
                   c(1),  # 90
                   c(1),  # 90
                   c(1),  # 90
                   c(1),  # 90
                   
                   c(1),  # 90
                   c(1),  # 90
                   c(1),  # 90
                   c(50, 35, 20, 10, 1),  # 41 
                   c(50, 35, 20, 10, 1),   # 47 
                   
                   c(50, 35, 20, 10, 1),   # 43 
                   c(50, 35, 20, 10, 1),   # 36 
                   c(1),   # 22
                   c(1),   # 22
                   c(1),   # 22
                   
                   c(1),   # 12
                   c(1),   # 14
                   c(1),   # 12
                   c(1),   # 12
                   c(1),   # 57
                   
                   c(1),   # 22
                   c(1),   # 22
                   c(1),   # 22
                   c(1),   # 22
                   c(1),   # 22
                   
                   c(1),   # 22
                   c(1),   # 22
                   c(1))   # 22
  
  for (i in 29:32) {
    
    raster <- exp(c.stack[[i]])
    title <- paste(st[i])
    sr_breaks <- leg_lab2[[i]]
    legend_title <- lt[[i]]
    
    obs_v11(raster, title, sr_breaks, legend_title)
  }
# --------------------------------------------------------------------------

  