######################################################
# Sue -- clay no clay
######################################################

# date created: 29/4/19
# last updated: 


# aim: see what this clay layer does, replace clay with it, and test with both rarefaction methods (I guess?)

# library ------------------------------------------------------------------
  library(broom)
  library(magrittr)
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

  rm(list = ls())
  
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results")
  
# data ----------------------------------------------------------------   
# environmental data (retained from varaible selection process)
# note this is also the order these pop up in the coefficient plots, so vary it here if need be
# we have replaced clay with CLAY
  vs.evs <- c("cell.category.v2", "prop.cover", "pcoldq", "pwarmq", "amt", "ts", "arid", "clay", "th", "hii")
  
# select from wider EV list
  evs <- read.csv("EVs/CSV/100 km EFs scaled.csv", header = T) %>%
    dplyr::select(vs.evs) %>%
    mutate(prop.cover = prop.cover/100) # convert prop cover from 0 - 1 
  
# rarefied data (generated from 'Plot' script)
  spp <- read.csv("Rarefaction/CSV/Rarefied 15 richness and record number pp independent.csv", header = T) %>%
    dplyr::select(Native.C3.rich, Native.C4.rich, 
                  Exotic.C3.rich, Exotic.C4.rich)
  
  colnames(spp) <- c("n.c3", "n.c4", 
                     "e.c3", "e.c4")
  head(spp)
  
# bind spp and EV data and subset to terrestrial only cells   
  spp.ev <- cbind(evs, spp) %>%
    filter(cell.category.v2 == "land") %>%
    dplyr::select(-cell.category.v2)
  
# models --------------------------------------------------------------
# native C3  
  m.n.c3 <- lm(n.c3 ~ pcoldq + pwarmq + amt + ts + arid + clay + th + hii + prop.cover, data = spp.ev)
  summary(m.n.c3) 
  m.n.c3.sum <- tidy(m.n.c3)
  m.n.c3.ci <- confint(m.n.c3)
  
# native C4
  m.n.c4 <- lm(n.c4 ~ pcoldq + pwarmq + amt + ts + arid + clay + th + hii + prop.cover, data = spp.ev)
  summary(m.n.c4) 
  m.n.c4.sum <- tidy(m.n.c4)
  m.n.c4.ci <- confint(m.n.c4)
  
# exotic C3  
  m.e.c3 <- lm(e.c3 ~ pcoldq + pwarmq + amt + ts + arid + clay + th + hii + prop.cover, data = spp.ev)
  summary(m.e.c3) 
  m.e.c3.sum <- tidy(m.e.c3)
  m.e.c3.ci <- confint(m.e.c3)
  
# exotic C4
  m.e.c4 <- lm(e.c4 ~ pcoldq + pwarmq + amt + ts + arid + clay + th + hii + prop.cover, data = spp.ev)
  summary(m.e.c4) 
  m.e.c4.sum <- tidy(m.e.c4)
  m.e.c4.ci <- confint(m.e.c4)
  
# coefficient dataframe --------------------------------------------
# note we are dropping prop.cover
# Status (n/e) | coefficient (evs) | mean | lower.ci | upper.ci
  spp.ev.mat <- matrix(ncol = 5, nrow = 16)  
  spp.ev.mat[,1] <- rep(c("native", "exotic"), each = 8)
  spp.ev.mat[,2] <- rep(vs.evs[3:10], 2)
  colnames(spp.ev.mat) <- c("status", "coef", "estimate", "lower.ci", "upper.ci")
  
# One for each total, C3 and C4 richness (i.e. native and exotic will be plotted together) 
  c3.rich <- spp.ev.mat
  c4.rich <- spp.ev.mat
  
# insert coeffficents & CIs --------------------------------
# 2:9 excludes the intercept and prop.cover terms
# C3 native   
  c3.rich[1:8, 3] <- m.n.c3.sum$estimate[2:9]
  c3.rich[1:8, 4:5] <- m.n.c3.ci[2:9, ]
# C3 exotic 
  c3.rich[9:16, 3] <- m.e.c3.sum$estimate[2:9]
  c3.rich[9:16, 4:5] <- m.e.c3.ci[2:9, ]
  
# C4 native  
  c4.rich[1:8, 3] <- m.n.c4.sum$estimate[2:9]
  c4.rich[1:8, 4:5] <- m.n.c4.ci[2:9, ]
# C4 exotic  
  c4.rich[9:16, 3] <- m.e.c4.sum$estimate[2:9]
  c4.rich[9:16, 4:5] <- m.e.c4.ci[2:9, ]
  
# data frame   
  c3.rich <- data.frame(c3.rich)
  c4.rich <- data.frame(c4.rich)
  
# Mean and CIs as numbers for plotting  
  c3.rich[,3:5] %<>% lapply(function(x) as.numeric(as.character(x)))
  c4.rich[,3:5] %<>% lapply(function(x) as.numeric(as.character(x)))
  
# ----------------------------------------------------------------------------
  
# plot here and now the estimates --------------------------------------------  
# EV order rearrange: to do so, change at the beginning of 'Raster Chapter model v7[current]' script   
# environmental variable labels
  ev.levels <- c("pcoldq", "pwarmq", "amt", "ts", "arid", "clay", "th", "hii")
  
  ev.labels <- c("Winter \nrainfall", 
                 "Summer \nrainfall", 
                 "Annual mean \ntemperature", 
                 "Temperature \nseasonality", 
                 "Aridity", 
                 "Clay (%)", 
                 "Topographic \nheterogeneity", 
                 "Human \nactivity")
  
# reorder levels in spp dfs to match EV labels
  c3.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  c3.rich$plot.names <- fct_inorder(c3.rich$plot.names)
  
  c4.rich$plot.names <- factor(ev.labels, ordered = is.ordered(ev.labels))
  c4.rich$plot.names <- fct_inorder(c4.rich$plot.names)
  
# C3 -------------------------------------------------------------------
# c3 adjusted r2 & position: x = 7, y = 1.5, label = "Adj. r2 = exotic 0.76, native 0.74"
  i <- ggplot(c3.rich, aes(x = plot.names, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Exotic", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental and anthropogenic variables", y = "Parameter estimate", color = "grey100") +
    geom_point(aes(y = estimate), size = 5, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 0.6)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = 1.5, label = "Adj. r2 = exotic 0.76, native 0.74")
  
  #ggThemeAssistGadget(h)
  
  i + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Rarefaction/Sue/Graphs/Clay plots/C3 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
# C4 ---------------------------------------------------------------
# c4 position and r2: x = 7, y = 3.1, label = "Adj. r2 = exotic 0.37, native 0.72"
  j <- ggplot(c4.rich, aes(x = plot.names, color = status)) +
    theme_classic() +
    scale_color_manual(labels = c("Exotic", "Native"), values = c("blue", "red")) +
    geom_hline(aes(yintercept = 0),
               color = "black", size = 0.6) +
    labs(title = "", x = "Environmental and anthropogenic variables", y = "Parameter estimate", color = "grey100") +
    geom_point(aes(y = estimate), size = 5, position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                  size = 1, width = 0, position = position_dodge(width = 0.6)) +
    annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf) +
    annotate("segment", x = -Inf, xend = -Inf,y = -Inf, yend = Inf) + 
    annotate("text", x = 7, y = 2.9, label = "Adj. r2 = exotic 0.35, native 0.71")
  
  #ggThemeAssistGadget(h)
  
  j + theme(axis.text = element_text(size = 12, colour = "gray0"),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 12), 
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 14), 
            legend.title = element_blank(), 
            axis.ticks.length = unit(0.2, "cm")) 
  
  ggsave("Rarefaction/Sue/Graphs/Clay plots/C4 coefficients.jpeg", plot = last_plot(), scale = 1, dpi = 500, device = "jpeg")
  
  
  
# -------------------------------------------------------------------------    

