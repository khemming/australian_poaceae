

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
  library(magrittr)
  library(ggtext)

  rm(list = ls())

# load  workspace from gls models -------------------------------------
  load("data/Rdata/models.RData")
  
# sub tribe names  
  st_names <- read.csv("results/csv/subtribe names.csv", sep = "_")
  head(st_names)
  nrow(st_names)
  
# r2 
  r2 <- read.csv("results/csv/r2.csv")
  r2
# model estimate data frames ----------------------------------------------------
# order of variables in models:
# amt + arid + ts + pwarmq + pcoldq+ th + clay
  plot_names <-  c("Annual mean temperature", 
                   "Aridity", 
                   "Temperature seasonality", 
                   "Summer rainfall", 
                   "Winter rainfall",
                   "Topographic heterogeneity",
                   "Percent clay")

# remove intercept and prop cover terms, reorder
  model_df <- function(ci_ls){
    m_df <- data.frame(ci_ls)
    m_df2 <- m_df %>% 
             mutate(pv = rownames(m_df),
                    estimate = est.) %>%
                         slice(2:8) %>%
             mutate(plot_names = factor(plot_names, levels = rev(plot_names))) %>%
             dplyr::select(plot_names, pv, lower, estimate, upper)
    return(m_df2)
    }
  
# list to store multiple data frames   
  plotting_list <- list()
  
# run function
  for (i in 1:nrow(st_names)){
    
    plotting_list[[i]] <- model_df(ci_ls[[i]])
    
  }
  names(plotting_list) <- st_names$names
  names(plotting_list)

# individual plot function ---------------------------------------
  coef_plot <- function(dat, subtribe, x_mn, x_mx, brks){
     q <- ggplot(dat, aes(y = plot_names)) +
        theme_classic() +
        geom_vline(aes(xintercept = 0),
                   colour = "black", 
                   size = 0.9, 
                   linetype = "dashed") +
        labs(colour = "Subtribe",
             x = "Mean estimate",
             y = "",
             title = subtribe) +
        geom_point(aes(x = estimate), size = 3.5) +
        geom_errorbarh(aes(xmin = lower, xmax = upper),
                       size = 1, height = 0) +
        scale_x_continuous(limits = c(x_mn, x_mx),
                           breaks = brks,
                           labels = brks) +
      #annotate("text", x = x_mn, y = 1.5, label = r2_lab, size = 4.5, hjust = 0) +
      theme(axis.title = element_text(size = 14),
            plot.title = element_text(size = 16),
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 14),
            axis.text.y = element_text(colour = "black", size = 12),
            axis.ticks.length = unit(0.2, "cm"),
            axis.ticks = element_line(size = 1, colour = "black"))
        save <- paste0("results/coefficient plots/", subtribe, ".jpeg")
        ggsave(save, plot = last_plot(), width = 13, height = 9, units = "cm", 
               dpi = 500, device = "jpeg")
      return(q) 
    
  } # coef_v2 end
# ----------------------------------------------------------------
  
# run function via loop --------------------------------------
# required: 
# dat     = model plotting dataframe (held in a list = output)
# subtribe  = family name (without status) / names22
# x_mn/x_mx = x axis margins; rounded max and min of CIs 
# breaks = formatted distance between mean estimates
# -------------------------------------------------------------
# notes  ----------------------------------------------
# breaks made using the following code to determine optimum distances:
# breaks = c(x_mn, round(x_mn/2, 2), 0, round(x_mx/2, 2), x_mx),
# labels = c(x_mn, round(x_mn/2, 2), 0, round(x_mx/2, 2), x_mx)
# breaks
  breaks <- list(c(-0.4, -0.2, 0, 0.2), 
                 c(-0.4, -0.2, 0, 0.2),
                 c(-0.3, 0, 0.3, 0.6),
                 c(-0.9, -0.6, -0.3, 0, 0.3),
                 c(-0.4, -0.2, 0, 0.2, 0.4, 0.6),  # 5
                 
                 c(-0.6, -0.3, 0, 0.3, 0.6), 
                 c(-0.5, 0, 0.5, 1, 1.5),
                 c(-0.8, -0.4, 0, 0.4, 0.8),
                 c(-0.4, -0.2, 0, 0.2),
                 c(-0.3, -0.15, 0, 0.15, 0.3),    # 10
                 
                 c(-0.6, -0.3, 0, 0.3),
                 c(-0.3, -0.15, 0, 0.15, 0.3),
                 c(-0.4, -0.2, 0, 0.2, 0.4),      
                 c(-0.3, -0.15, 0, 0.15, 0.3),
                 c(-0.6, -0.3, 0, 0.3, 0.6),      # 15   
                 
                 c(-0.3, -0.15, 0, 0.15, 0.3),    
                 c(-0.9, -0.6, -0.3, 0, 0.3, 0.6),   
                 c(-0.2, -0.1, 0, 0.1, 0.2),   
                 c(-0.3, -0.15, 0, 0.15, 0.3),           
                 c(-0.3, -0.15, 0, 0.15, 0.3),    # 20
                 
                 c(-0.6, -0.3, 0, 0.3, 0.6),
                 c(-0.6, -0.3, 0, 0.3, 0.6),
                 c(-1.2, -0.6, 0, 0.6, 1.2),
                 c(-0.9, -0.6, -0.3, 0, 0.3),    
                 c(-0.6, -0.3, 0, 0.3, 0.6, 0.9),    # 25
                 
                 c(-0.5, -0.25, 0, 0.25, 0.5),
                 c(-0.6, -0.3, 0, 0.3, 0.6),
                 c(-0.3, -0.15, 0, 0.15, 0.3),
                 c(-4, -3, -2, -1, 0, 1, 2),# height begins
                 c(-4, -3, -2, -1, 0, 1, 2),   # 30
                 
                 c(-5, -4, -3, -2, -1, 0, 1, 2, 3),   
                 c(-5, -4, -3, -2, -1, 0, 1, 2, 3),# h ends
                 c(-0.3, -0.15, 0, 0.15, 0.3),            
                 c(-0.3, 0, 0.3, 0.6, 0.9, 1.2),
                 c(-0.3, -0.15, 0, 0.15, 0.3, 0.45),   # 35
                 
                 c(-0.5, -0.25, 0, 0.25),
                 c(-0.3, -0.15, 0, 0.15, 0.3),
                 c(-0.3, -0.15, 0, 0.15, 0.3),
                 c(-0.6, -0.3, 0, 0.3, 0.6),   
                 c(-0.5, -0.25, 0, 0.25),             # 40
                 
                 c(-0.6, -0.3, 0, 0.3, 0.6),
                 c(-0.9, -0.6, -0.3, 0, 0.3, 0.6),
                 c(-0.6, -0.3, 0, 0.3, 0.6),   
                 c(-0.6, -0.3, 0, 0.3, 0.6),
                 c(-0.9, -0.6, -0.3, 0, 0.3, 0.6, 0.9),            # 45
                 
                 c(-0.4, -0.2, 0, 0.2, 0.4),
                 c(-0.3, -0.15, 0, 0.15, 0.3, 0.45),
                 c(-0.3, -0.15, 0, 0.15, 0.3, 0.45)
                 )
  
# -------------------------------------------------------------
# check

# number one
  i <- 48
  coef_plot(dat <- plotting_list[[i]],
            subtribe <- st_names[i, ],
            floor(min(plotting_list[[i]]$lower) * 10)/10,
            ceiling(max(plotting_list[[i]]$upper) * 10)/10,
            breaks[[i]])
  
# run for all sub-tribes  
  for (i in 1:nrow(st_names)) {
    
    coef_plot(data.frame(plotting_list[[i]]),
              st_names[i, ],
              x_mn <- floor(min(plotting_list[[i]]$lower) * 10)/10,
              x_mx <- ceiling(max(plotting_list[[i]]$upper) * 10)/10,
              breaks[[i]])
    
  }
  
  
# ---------------------------------------------------------  

# grouped plots --------------------------------------------------
# grouped plots:
# all: C3 & C4
# native/endemic: by subtribe
# & micrairoides
  coef_v1 <- function(title, a, b, x_limits, x_labels){
    
    plotting_list[[a]]$Subtribe <- a
    a_df <- plotting_list[[a]]
    plotting_list[[b]]$Subtribe <- b
    b_df <- plotting_list[[b]]
    dat <- bind_rows(a_df, b_df)
    dat
    
    r2_lab <- paste0(a, " R2 = ", r2[r2$sub_tribe == a, 2], "\n", 
                     b, " R2 = ", r2[r2$sub_tribe == b, 2])
    
    q <-  ggplot(dat, aes(y = plot_names, shape = Subtribe, colour = Subtribe)) +
      theme_classic() +
      scale_colour_manual(labels = c(a, b),
                          values = c("blue", "red")) + 
      geom_vline(aes(xintercept = 0),
                 colour = "black", 
                 size = 0.9, 
                 linetype = "dashed") +
      labs(colour = "Subtribe",
           shape = "Subtribe",
           x = "Mean estimate",
           y = "",
           title = title,
           caption = r2_lab) +
      geom_point(aes(x = estimate), size = 3.5, position = position_dodge(width = 0.6)) +
      geom_errorbarh(aes(xmin = lower, xmax = upper),
                     size = 1, height = 0, position = position_dodge(width = 0.6)) +
      scale_x_continuous(limits = x_limits,
                         breaks = x_labels,
                         labels = x_labels) +
    # annotate("text", size = 4, x = max(x_limits)/1.5, y = 1, label = r2_lab) +
      theme(legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.position = "bottom", 
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 14),
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 14),
            axis.text.y = element_text(colour = "black", size = 14),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1),
            plot.caption = element_text(size = 14))
    
    save <- paste0("Results/coefficient plots/", title, ".jpeg")
    ggsave(save, plot = last_plot(), width = 15, height = 12, units = "cm", dpi = 500, device = "jpeg")
    return(q) }
  
# run for groups --------------------------------------------------
# coef_v1 = function(title, dat, x_limits, x_labels)
# add sub tribe label and create data frame
  coef_v1(title <- "All C3 and C4", 
          a <- "All C3",
          b <- "All C4",
          x_limits <- c(-0.9, 0.7), 
x_labels <- c(round(seq(-9, 6, 0.3), digits = 2)))
  
# Andropogoneae native/endemic
  coef_v1("Endemic-native Andropogoneae", 
          "Endemic Andropogoneae",
          "Native Andropogoneae",
          c(-0.45, 0.65), 
          c(round(seq(-3, 0.6, 0.3), digits = 2)))
  
# Chloridoideae native/endemic
  coef_v1("Endemic-native Chloridoideae", 
          "Endemic Chloridoideae",
          "Native Chloridoideae",
          c(-0.6, 0.6), 
          c(round(seq(-0.6, 0.6, 0.3), digits = 2)))
  
# Paniceae native/endemic
  coef_v1("Endemic-native Paniceae", 
          "Endemic Paniceae",
          "Native Paniceae",
          c(-0.65, 0.65), 
          c(round(seq(-0.6, 0.6, 0.3), digits = 2)))
  
# Early spp - shared v endemic
  coef_v1("Endemic-native Early", 
          "Early endemic spp",
          "Early native  spp",
          c(-0.66, 0.34), 
          c(round(seq(-0.6, 0.3, 0.3), digits = 2)))
  
# Mid spp - shared v endemic
  coef_v1("Endemic-Native Mid", 
          "Mid endemic spp",
          "Mid native spp",
          c(-0.5, 0.3), 
          c(round(seq(-0.5, 0.25, 0.25), digits = 2)))
  

# ----------------------------------------------------------------  
  