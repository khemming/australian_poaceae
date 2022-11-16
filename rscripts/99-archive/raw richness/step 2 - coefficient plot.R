

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
  library(magrittr)

  rm(list = ls())

# load  workspace from iNEXT model (script) -------------------------------------
  load("data/Rdata/raw richness models.RData")
  
# sub tribe names  
  st_names <- read.csv("results/csv/subtribe names.csv")
  nrow(st_names)
  
# model estimate data frames ----------------------------------------------------
# what order are the variables in?
  plot_names <-  c("Winter\nrainfall", 
                   "Summer\nrainfall", 
                   "Annual mean\ntemperature", 
                   "Temperature\nseasonality", 
                   "Aridity", 
                   "Topographic\nheterogeneity",
                   "Percent\nclay")

# remove intercept and prop cover terms, reorder
  model_df <- function(m, est){
    m_df <- data.frame(est)
    m_df2 <- m_df %>% 
             mutate(pv = rownames(m_df),
                    estimate = summary(m)$coef[,1],
                    lower = m_df$X2.5..,
                    upper = X97.5..) %>%
             slice(2:8) %>%
             mutate(plot_names = factor(plot_names, levels = rev(plot_names))) %>%
             dplyr::select(plot_names, pv, lower, estimate, upper)
    return(m_df2)
    }
  
# list to store multiple data frames   
  plotting_list <- list()
  
# run function
  for (i in 1:nrow(st_names)){
    
    plotting_list[[i]] <- model_df(lm_list[[i]], lm_est[[i]])
    
  }

# plot function ---------------------------------------
  coef_v2 <- function(title, dat, x_limits, x_labels, r2_i){
    r2_lab <- paste0("adj. R2 = ", round(r2_i, 3))
    q <- ggplot(dat, aes(y = plot_names)) +
        theme_classic() +
        geom_vline(aes(xintercept = 0),
                   colour = "black", 
                   size = 0.9, 
                   linetype = "dashed") +
        labs(colour = "Status",
             x = "Mean estimate",
             y = "",
             title = title,
             caption = r2_lab
             ) +
        geom_point(aes(x = estimate), size = 6) +
        geom_errorbarh(aes(xmin = lower, xmax = upper),
                       size = 1.5, height = 0.1) +
        scale_x_continuous(limits = x_limits,
                           breaks = x_labels,
                           labels = x_labels) +
        theme(legend.title = element_text(size = 18, face = "bold"),
              legend.text = element_text(size = 16),
              legend.position = "bottom", 
              axis.title = element_text(size = 18, face = "bold"),
              plot.title = element_text(size = 22, face = "bold"),
              axis.line = element_line(colour = "black", size = 1.5),
              axis.text.x = element_text(colour = "black", size = 18),
              axis.text.y = element_text(colour = "black", size = 14),
              axis.ticks.length = unit(0.2, "cm"),
              axis.ticks = element_line(size = 1),
              plot.caption = element_text(size = 16, face = "bold"))
        save <- paste0("Results/raw richness/", title, ".jpeg")
        ggsave(save, plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
      return(q) 
    
  } # coef_v2 end
# ----------------------------------------------------------------
  
# manual individual plots -----------------------------------------------  
# (1) All Andropogoneae
  coef_v2(title <- st_names[1,],
          dat <- data.frame(plotting_list[[1]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[1]])$adj.r.squared)
  
# (2) All C3
  coef_v2(title <- st_names[2,],
          dat <- data.frame(plotting_list[[2]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[2]])$adj.r.squared)
  
# (3) All C4
  coef_v2(title <- st_names[3,],
          dat <- data.frame(plotting_list[[3]]),
          x_limits <- c(-21, 25),
          x_labels <- seq(-20, 25, 5),
          summary(lm_list[[3]])$adj.r.squared)
  
# (4) All Chloridoideae
  coef_v2(title <- st_names[4,],
          dat <- data.frame(plotting_list[[4]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[4]])$adj.r.squared)
  
# (5) All Micrairoideae
  coef_v2(title <- st_names[5,],
          dat <- data.frame(plotting_list[[5]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[5]])$adj.r.squared)
  
# (6) All Paniceae
  coef_v2(title <- st_names[6,],
          dat <- data.frame(plotting_list[[6]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[6]])$adj.r.squared)
  
# (7) All species
  coef_v2(title <- st_names[7,],
          dat <- data.frame(plotting_list[[7]]),
          x_limits <- c(-16, 10),
          x_labels <- seq(-15, 10, 5),
          summary(lm_list[[7]])$adj.r.squared)
  
# (8) Endemic Andropogoneae
  coef_v2(title <- st_names[8,],
          dat <- data.frame(plotting_list[[8]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[8]])$adj.r.squared)
  
# (9) Endemic C3
  coef_v2(title <- st_names[9,],
          dat <- data.frame(plotting_list[[9]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[9]])$adj.r.squared)
  
# (10) Endemic C4
  coef_v2(title <- st_names[10,],
          dat <- data.frame(plotting_list[[10]]),
          x_limits <- c(-15, 20),
          x_labels <- seq(-15, 20, 5),
          summary(lm_list[[10]])$adj.r.squared)
  
# (11) Endemic Chloridoideae
  coef_v2(title <- st_names[11,],
          dat <- data.frame(plotting_list[[11]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[11]])$adj.r.squared)
  
# (12) Endemic Paniceae
  coef_v2(title <- st_names[12,],
          dat <- data.frame(plotting_list[[12]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[12]])$adj.r.squared)
  
# (13) Endemic species 
  coef_v2(title <- st_names[13,],
          dat <- data.frame(plotting_list[[13]]),
          x_limits <- c(-15, 20),
          x_labels <- seq(-15, 20, 5),
          summary(lm_list[[13]])$adj.r.squared)
  
# (14) Native Andropogoneae
  coef_v2(title <- st_names[14,],
          dat <- data.frame(plotting_list[[14]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[14]])$adj.r.squared)
  
# (15) Native Chloridoideae 
  coef_v2(title <- st_names[15,],
          dat <- data.frame(plotting_list[[15]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[15]])$adj.r.squared)
  
# (16) Native Paniceae 
  coef_v2(title <- st_names[16,],
          dat <- data.frame(plotting_list[[16]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[16]])$adj.r.squared)
  
# (17) Native species 
  coef_v2(title <- st_names[17,],
          dat <- data.frame(plotting_list[[17]]),
          x_limits <- c(-10, 10),
          x_labels <- seq(-10, 10, 5),
          summary(lm_list[[17]])$adj.r.squared)
# ---------------------------------------------------------  

# grouped plots --------------------------------------------------
# groups are:
# 