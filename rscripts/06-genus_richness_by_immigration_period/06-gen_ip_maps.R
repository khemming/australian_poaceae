
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
  library(dplyr)
  library(forcats)
  library(maps)
  
  rm(list = ls())
  
# data -----------------------------------------------------------------
# shape file
  oz <- readOGR("data/Australia/Australia shapefile.shp")
  plot(oz)
  
# rasters
  current.list <- list.files(path = "results/supplementary materials/genera",
                             pattern = ".grd", full.names = T)
  current.list
  st <- gsub(pattern = "results/supplementary materials/genera/|.grd$", "", current.list)
  st
  c.stack <- log(stack(current.list))
  c.stack <-  calc(c.stack, fun = function(x) {x[x<0] <- 0; return(x)})
  names(c.stack) <- st
  list2env(setNames(unstack(c.stack), names(c.stack)), .GlobalEnv)
  
# raw genus richness ------------------------------------
## for scale bar  
  j <- vector()
  
  for (i in 1:length(st)){
    
    j[i] <- ceiling(cellStats(exp(c.stack[[i]]), "max", na.rm = T))
    
  }
  j
  leg_lab <- list(c(15,  12, 8, 5,  1),   # 14
                  c(4, 3, 2, 1),          # 4
                  c(11, 8, 5, 1),       # 11
                  c(4, 3, 2, 1),      # 4
                  c(6, 4, 2, 1),       # 6
                  
                  c(5, 4, 3, 2, 1),  # 5
                  c(4, 3, 2, 1),  # 4
                  c(3, 2, 1),       # 3
                  c(7,  5, 3, 1),      # 7
                  c(8, 5,  3, 1),  # 8
                  
                  c(5, 4, 3, 2, 1),  # 5
                  c(35,  25, 10,  5, 1))  # 34
  
  names(leg_lab) <- st
  
# scale bar units
  lt <- c(rep("Genus\nrichness", 12))
  
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
    
  # genus richness colours
    colr <- rev(brewer.pal(11, "Spectral"))
  # save 
    save_txt <- paste0("results/maps/sub tribes/", title, " (genus).jpeg")  
    
  # plot
    q <- ggplot() +  
      ggtitle(gsub("_", " ", title)) +
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
# sr_breaks = the raw genus richness legend labels (i.e. leg_lab)
# sr_max = max genus richness
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
