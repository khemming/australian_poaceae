  
# library ---------------------------------------------------------------------
  library(RColorBrewer)
  library(purrr)
  library(ggthemes)
  library(corrplot)
  library(factoextra)
  library(tidyverse)
  
  rm(list = ls()) 
  
# data -----------------------------------------------------------------------
## from Poaceae project
  
# all predictor variables
  pv <- read.csv("C:/Users/s436862/Dropbox/Poaceae/Results/csv/predictor variables 2538.csv") %>%
    dplyr::select(-cell_id, -lat, -long, -cell_category, -proportion_cover, -hii)
  
# variable selection predictors
  vs <- pv %>% dplyr::select(pcoldq, pwarmq, amt, ts, arid, clay, th)
  
# correlation matrix -------------------------------------------------------
  pv_cor <- round(cor(pv, method = "pearson", use = "complete.obs"), 2)
  write.csv(pv_cor, "results/csv/all predictor variable correlation matrix.csv")  
    
  pcor_vs <- round(cor(vs, method = "pearson", use = "complete.obs"), 2)
  write.csv(pcor_vs, "Results/csv/variable selection correlation matrix.csv")  
    
# PCA ------------------------------------------------------------------------------
# complete EV data set
  pv2 <- pv %>% drop_na()
  res.pca <- prcomp(pv2, scale = F)
  
  fviz_pca_var(res.pca,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE)  + # Avoid text overlapping
    theme_classic() +
    labs(title = "All variables PCA") +
    xlab("Dimension 1 (35.0%)") +
    ylab("Dimension 2 (28.1%)") +
    theme(axis.ticks.length = unit(0.25, "cm"),
          axis.ticks = element_line(colour = "black", size = 1),
          axis.title = element_blank(),
          plot.title = element_text(size = 20),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 12),
          axis.text.y = element_text(colour = "black", size = 12))
  
  ggsave("results/supplementary materials/PCA - all vars.jpeg", plot = last_plot(), dpi = 500, width = 14, height = 13, units = "cm", device = "jpeg") 
    
    
# variable selection data set
  vs2 <- vs %>% drop_na()
  res.pca2 <- prcomp(vs2, scale = F)
  
  fviz_pca_var(res.pca2,
               col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE)  + # Avoid text overlapping
    theme_classic() +
    labs(title = "Selected variables PCA") +
    xlab("Dimension 1 (40.7%)") +
    ylab("Dimension 2 (28.4%)") +
    theme(axis.ticks.length = unit(0.25, "cm"),
          axis.ticks = element_line(colour = "black", size = 1),
          axis.title = element_blank(),
          plot.title = element_text(size = 20),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(colour = "black", size = 12),
          axis.text.y = element_text(colour = "black", size = 12))
  
  ggsave("results/supplementary materials/PCA - vs vars.jpeg", plot = last_plot(), dpi = 500, width = 14, height = 13, units = "cm", device = "jpeg") 
  
# ------------------------------------------------------------------