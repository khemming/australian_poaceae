


# library ------------------------------------------------
  library(tidyverse)
  library(ggThemeAssist)
  library(raster)
  library(gplots)
  library(RColorBrewer)
  library(ggmap)
  library(rgdal)
  library(rasterVis)
  library(maptools)
  library(ggthemes)
  library(forcats)
  library(maps)
  library(oz)
  library(rgeos)


rm(list = ls())

# data ---------------------------------------------------
# note: from Poaceae chapter

# shape file
  oz <- readOGR("C:/Users/s436862/Dropbox/Poaceae/Data files/Australia/Australia shapefile.shp")
  plot(oz)

# rasters
  setwd("C:/Users/s436862/Dropbox/Poaceae/Results/rasters/predictor variables")
  files <- list.files(pattern = ".grd")
  short_names <- gsub(pattern = ".grd", "", files)
  stack <- stack(files)
  names(stack) <- short_names
  setwd("C:/Users/s436862/Dropbox/Projects/Sue Bryceson")
  
  short_names_df <- data.frame(short_names)
  
  title_names <- c("Annual mean temperature",
                   "Annual precipitation",
                   "Aridity",
                   "Clay",
                   "Elevation",
    
                   "Human influence index",
                   "Isothermality",
                   "Mean diurnal range",
                   "Plant available water capacity",
                   "Precipitation of the coldest quarter",
    
                   "Precipitation of the driest month",
                   "Precipitation of the driest quarter",
                   "Plant-extractable the water capacity",
                   "Precipitation seasonality",
                   "Precipitation of the warmest quarter",
                   
                   "Precipitation of the wettest month",
                   "Precipitation of the wettest quarter",
                   "Potential storage of water in the root zone",
                   "Potential storage of water in the soil profile",
                   "Potential storage of water derived from soil texture",
                   
                   "Temperature annual range",
                   "Minimum temperature of the coldest Month",
                   "Mean temperature of the coldest quarter",
                   "Mean temperature of driest quarter",
                   "Topographic heterogeneity",
                   
                   "Temperature seasonality",
                   "Maxmimum temperature of the warmest month",
                   "Mean temperature of the warmest quarter",
                   "Mean temperature of the wettest quarter")
  
# insert units here
  units <- c("Degrees C", "mm", "", "Percent (%)", "M", 
             "Index", "SD", "Range", "mm", "mm", 
             "mm", "mm", "mm", "SD", "mm",
             "mm", "mm", "mm", "mm", "mm",
             "Range", "Degrees C", "Degrees C", "Degrees C", "SD",
             "SD", "Degrees C", "Degrees C", "Degrees C")
  names_df <- cbind(short_names, title_names, units)
  names_df <- data.frame(names_df)
  
# plot function ------------------------------------------------------------------------
# raster = raster
# save = save file name
# title = plot title
  ras_v7 <- function(raster, title, units){
    
    # raster to spatial points dataframe 
    raster_spdf <- as(raster, "SpatialPixelsDataFrame")
    raster_df <- as.data.frame(raster_spdf)
    colnames(raster_df) <- c("value", "x", "y")
    # pretty colours
    colr <- rev(brewer.pal(11, "Spectral"))
    # save 
    save_txt <- paste0("Results/maps/predictor variables/", title, ".jpeg")
    # plot
    q <- ggplot() +  
      ggtitle(title) +
      geom_polygon(data = oz, aes(x = long, y = lat, group = group), 
        fill = "grey60") +                                               
      geom_tile(data = raster_df, aes(x = x, y = y, fill = value)) +             
      geom_polygon(data = oz, colour = "grey1", 
        aes(x = long, y = lat, group = group), fill = NA, size = 0.7) + 
      scale_fill_gradientn(colours = colr, 
        #limits = c(0, 1),                             
        #breaks = c(0, 0.5, 1),
        space = "Lab",
        name = units) +
      coord_fixed(xlim = c(112, 155), ylim = c(-45, -7), expand = F) +
      theme_map() +
      theme(legend.direction = "vertical",
        legend.justification = "right",
        legend.position = "right",
        legend.key.size = unit(12, "cm"),
        legend.key.width = unit(1,"cm"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.box.spacing = unit(0.1, "cm"),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.1, vjust = -0.5)) +
      guides(fill = guide_colorbar(barheight = unit(5, "cm"))) 
    plot(q)
    
    ggsave(save_txt, plot = last_plot(), height = 14, width = 16, units = "cm", dpi = 500, device = "jpeg")
  } # finish function
  
# maps ------------------------------------------------------------------
# raster = raster
# title = plot title
# units = names_df$units
  
  for (i in 1:nrow(names_df)) {
    
    ras_v7(stack[[i]],
           title_names[i],
           names_df$units[i])
    
  }
  
# -------------------------------------------------------------------------
  
  
  
  
  
  
  
    