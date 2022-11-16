

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
  oz <- readOGR("data/Australia/Australia shapefile.shp")
  plot(oz)
  
# rasters
  setwd()
  current.list <- list.files(path = "results/rasters",
                             pattern = ".grd", full.names = T)
  st <- gsub(pattern = "results/rasters/|.grd$", "", current.list)
  c.stack <- log(stack(current.list))
  c.stack <-  calc(c.stack, fun = function(x) {x[x<0] <- 0; return(x)})
  names(c.stack) <- st
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# scaled maps -------------------------------------------------------
  ras_v9 <- function(raster, title, sr_breaks, sb_max, sr_max, legend_title){
    
  # the highest log richness between the scale bar and raster
    mx <- max(sb_max, sr_max)
  
  # log transform scale bar
    sr_br_l <- log(sr_breaks)
    
  # scale species via highest species richness
    scale_sr <- log_sr/sr_max
    
    scale_mx <- mx/max(log_sr)
    
    leg_scale <- scale_sr / scale_mx
    
  # spatial points dataframe 
    raster2 <- raster
    raster_spdf <- as(raster2, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
  # km scale bar  
    # km_scale <- raster_df %>% rename(lat = y, long = x)
    # km_pos <- as.vector(data.frame(x = 130, 
    #                                y = -40))
    
  # pretty colours
    colr <- rev(brewer.pal(11, "Spectral"))
  # save 
    save_txt <- paste0("results/maps/sub tribes/test/", title, ".jpeg")  
    
    # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = oz, aes(x = long, y = lat, group = group), 
                   fill = "grey60") +                                               
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = oz, colour = "grey1", 
                   aes(x = long, y = lat, group = group), fill = NA, size = 0.7) + 
      scale_fill_gradientn(colours = colr, 
                           limits = c(min(log_sr), mx),            
                           breaks = log_sr, 
                           labels = sr_breaks,
                           space = "Lab",
                           name = legend_title) +
      coord_fixed(ratio = 38/33, xlim = c(112, 155), ylim = c(-45, -7)) +
      #ggsn::scalebar(km_scale, dist = 500, dist_unit = "km",  st.size = 4, st.dist = 0.05, st.bottom = T, height = 0.05, transform = T, model = 'WGS84', location = "topright", anchor = km_pos)
      theme_map() +
      theme(legend.direction = "vertical",
            legend.justification = "right",
            legend.position = "right",
            legend.key.size = unit(18, "cm"),
            legend.key.width = unit(1,"cm"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            legend.box.spacing = unit(0.01, "cm"),
            plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)) +
      guides(fill = guide_colorbar(barheight = unit(5, "cm")))
    
    plot(q)
    
    ggsave(save_txt, plot = last_plot(), width = 15, height = 15, units = "cm", dpi = 500, device = "jpeg")
  } # finish function

# scale bar --------------------------------------------------------------
# scaled log richness by nice incredments of raw richness
  for (i in 1:length(st)) {
   j <- exp(c.stack)
    
  }
      
# for native then nonnative richness
  leg_lab <- list(c(40,  20,  5,  1), # All Andropogoneae
                  c(50,  25,  5,  1), # All C3
                  c(100, 50, 10, 1),  # All C4
                  c(60,  40, 10,  1), # All Chloridoideae
                  c(20,  10, 5,  1),  # All Micrairoideae
                  
                  c(30,  20, 5,  1),  # All Paniceae
                  c(120, 90, 20, 5),  # All species
                  c(25,  10, 5,  1),  # Endemic Andropogoneae
                  c(50,  25, 5,  1),  # Endemic C3
                  c(80,  50, 10, 1),  # Endemic C4
                  
                  c(40,  20,  5,   1),  # Endemic Chloridoideae
                  c(50,  25,  5,   1),  # Endemic Paniceae
                  c(90,  60,  20,  5),  # Endemic Species
                  c(200, 150, 75, 25),  # Height All
                  c(150, 120, 60, 10),  # Height C4
                  
                  c(25,  10,  5,  1),  # Native Andropogoneae
                  c(15,  10,  5,  1),  # Native Chloridoideae
                  c(15,  10,  5,  1),  # Native Paniceae
                  c(60,  40, 10,  1),  # Native species
                  c(12,  8,   4,  1))  # Paleo C3
              
  names(leg_lab) <- st
  
# legend title: height has its own scale  
  lt <- c(rep("Species\nrichness", 13), 
          rep("Height\n(cm)",         2), 
          rep("Species\nrichness", 5))
  
# plots --------------------------------------------------------------------
  for (i in 1:length(st)) {
    
    raster <- c.stack[[i]]
    title <- paste(st[i])
    sr_breaks <- leg_lab[[i]]
    sb_max <- max(log(leg_lab[[i]]))
    sr_max <- cellStats(c.stack[[i]], "max", na.rm = T)
    legend_title <- lt[i]
    
    ras_v9(raster, title, sr_breaks, sb_max, sr_max, legend_title)
    
  }
# --------------------------------------------------------------------------
