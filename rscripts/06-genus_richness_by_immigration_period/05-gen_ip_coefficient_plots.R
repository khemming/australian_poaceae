

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
  library(ggplot2)
  library(forcats)
  library(maps)
  library(magrittr)
  library(ggtext)

  rm(list = ls())

# load  workspace from iNEXT model (script) -------------------------------------
  load("data/supplementary materials/genera gls model data.RData")
  
# sub tribe names  
  st_names <- data.frame(names = c("Ancient", "C3_recent", "Early_endemic", "Early_native", 
                                   "Homegrown_C4s", "Mid_endemic", "Mid_native"))
  head(st_names)
  nrow(st_names)
  
# r2 
  r2 <- read.csv("results/supplementary materials/genera/r2.csv")
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
  coef_plot <- function(dat, subtribe, x_mn, x_mx, brks, r2_i){
    
    q <- ggplot(dat, aes(y = plot_names)) +
        theme_classic() +
        geom_vline(aes(xintercept = 0),
                   colour = "black", 
                   size = 0.9, 
                   linetype = "dashed") +
        labs(colour = "Subtribe",
             x = "Mean estimate",
             y = "",
             title = gsub("_", " ", subtribe)) +
        geom_point(aes(x = estimate), size = 3.5) +
        geom_errorbarh(aes(xmin = lower, xmax = upper),
                       size = 1, height = 0) +
        scale_x_continuous(limits = c(x_mn, x_mx),
                           breaks = brks,
                           labels = brks) +
      theme(axis.title = element_text(size = 14),
            plot.title = element_text(size = 16),
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 14),
            axis.text.y = element_text(colour = "black", size = 12),
            axis.ticks.length = unit(0.2, "cm"),
            axis.ticks = element_line(size = 1, colour = "black"))
        save <- paste0("results/coefficient plots/", subtribe, " (genus).jpeg")
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
  breaks <- list(c(-0.3, -0.15, 0, 0.15, 0.3),         # Ancient
                 c(-0.3, -0.15, 0, 0.15, 0.3),         # C3 recent
                 c(-0.2, -0.1, 0, 0.1, 0.2),       # Early endemic
               
                 c(-0.15, 0, 0.15, 0.3),     # Early native
                 c(-0.3, -0.15, 0, 0.15, 0.3),          # Home grown
                 c(-0.3, -0.15, 0, 0.15, 0.3),       # Mid endemic
                 
                 c(-0.3, -0.15, 0, 0.15, 0.3))  # Mid native 
  
# -------------------------------------------------------------
# check

# number one
  i <- 7
  coef_plot(dat <- plotting_list[[i]],
            subtribe <- st_names[i, ],
            floor(min(plotting_list[[i]]$lower) * 10)/10,
            ceiling(max(plotting_list[[i]]$upper) * 10)/10,
            breaks[[i]],
            sprintf("%.3f", r2[i, 2]))
  
# run for all sub-tribes  
  for (i in 1:nrow(st_names)) {
    
    coef_plot(data.frame(plotting_list[[i]]),
              st_names[i, ],
              x_mn <- floor(min(plotting_list[[i]]$lower) * 10)/10,
              x_mx <- ceiling(max(plotting_list[[i]]$upper) * 10)/10,
              breaks[[i]],
              sprintf("%.3f", r2[i, 2]))
    
  }
  
  
# ---------------------------------------------------------  

# grouped plots --------------------------------------------------
  coef_v1 <- function(title, a, b, x_limits, x_labels){
    
    a1 <- gsub("_", " ", a, fixed = TRUE)
    b1 <- gsub("_", " ", b, fixed = TRUE)
    
    a_df <- plotting_list[[a]] %>% 
            mutate(Subtribe = a1)
    b_df <- plotting_list[[b]]  %>% 
            mutate(Subtribe = b1)
    dat <- bind_rows(a_df, b_df)
    dat
    
    q <-  ggplot(dat, aes(y = plot_names, shape = Subtribe, colour = Subtribe)) +
      theme_classic() +
      scale_colour_manual(labels = c(a1, b1),
                          values = c("blue", "red")) + 
      geom_vline(aes(xintercept = 0),
                 colour = "black", 
                 size = 0.9, 
                 linetype = "dashed") +
      labs(colour = "Subtribe",
           shape = "Subtribe",
           x = "Mean estimate",
           y = "",
           title = title) +
      geom_point(aes(x = estimate), size = 3.5, position = position_dodge(width = 0.6)) +
      geom_errorbarh(aes(xmin = lower, xmax = upper),
                     size = 1, height = 0, position = position_dodge(width = 0.6)) +
      scale_x_continuous(limits = x_limits,
                         breaks = x_labels,
                         labels = x_labels) +
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
    
    q
    save <- paste0("Results/coefficient plots/", title, " (genus).jpeg")
    ggsave(save, plot = last_plot(), width = 15, height = 12, units = "cm", dpi = 500, device = "jpeg")
    return(q) }
  
# run for groups --------------------------------------------------
# Early - n vs. e
# Mid - n vs. e
  
# coef_v1 = function(title, dat, x_limits, x_labels)
# add sub tribe label and create data frame
  coef_v1("Early native-endemic", 
          "Early_endemic",
          "Early_native",
          c(-0.2, 0.25), 
          c(round(seq(-0.2, 0.2, 0.1), digits = 2)))
  
# Andropogoneae native/endemic
  coef_v1("Mid native-endemic", 
          "Mid_endemic",
          "Mid_native",
          c(-0.26, 0.25), 
          c(round(seq(-0.2, 0.2, 0.1), digits = 2)))

# ----------------------------------------------------------------  
  