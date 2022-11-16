# Date created: 14/1/19
# Last updated: 


# 1.0 Native AVH data ---------------------------------------------------------------
# Sending Sue my native data for a look at

# Library
  library(ggplot2)
  library(dplyr)
  library(ggthemes)
  library(ggThemeAssist)
  library(tidyr)
  
  setwd("C:/Users/s436862/Dropbox/Climate Matching/Data files/Osborne C3-C4")
  
  spp.df <- read.csv("AVH grass pp.csv") %>% 
              filter(status == "native") %>%
              select(species, pp)
              
  write.csv(spp.df, "AVH native pp.csv", row.names = F)

# 2.0 Cut-off reduction in resolution scatterpols -------------------------------------

# Library
  library(ggplot2)
  library(dplyr)
  library(ggthemes)
  library(ggThemeAssist)
  library(tidyr)

  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/CSV")

# Species data 
  rare <- read.csv("Native multiple cutoff COMPLETE.csv") %>%
                    select(total.rare.15, total.rare.20, total.rare.25, total.rare.30,
                           total.rare.35, total.rare.40, total.rare.45, total.rare.50)
  

# Scatterplot function -------------------------------------------------------
# Here we are interested in how the ideal (50 records) correlates with progressively lower-resolution cutoffs in terms of maximising upper-species richness limit
  setwd("C:/Users/s436862/Dropbox/Rarefaction/Results/Rarefaction/Graphs/Rarefaction cutoff scatterplots")

# Requires: x, y, xlab, ylab, save
  scat_fun <- function (x, y, xlab, ylab, save)  {
    a <- ggplot(aes(x = x, y = y), data = rare) +
      geom_point(size = 1.5) +
      theme_bw() + 
      labs(x = xlab,
           y = ylab) +
      theme(axis.title = element_text(size = 14)) + 
      geom_smooth(method = "lm", se = FALSE)
    ggsave(save, plot = last_plot(), dpi = 500, device = "jpeg")
    
    a
  } # function end
  
# 15 records
  scat_fun(rare$total.rare.50, 
           rare$total.rare.15, 
           "50-record rarefaction", 
           "15-record rarefaction", 
           "15-50 records.jpeg")
# 20 records
  scat_fun(rare$total.rare.50, 
           rare$total.rare.20, 
           "50-record rarefaction", 
           "20-record rarefaction", 
           "20-50 records.jpeg")

# 25 records
  scat_fun(rare$total.rare.50, 
           rare$total.rare.25, 
           "50-record rarefaction", 
           "25-record rarefaction", 
           "25-50 records.jpeg")

# 30 records
  scat_fun(rare$total.rare.50, 
           rare$total.rare.30, 
           "50-record rarefaction", 
           "30-record rarefaction", 
           "30-50 records.jpeg")

# 35 records
  scat_fun(rare$total.rare.50, 
           rare$total.rare.35, 
           "50-record rarefaction", 
           "35-record rarefaction", 
           "35-50 records.jpeg")

# 40 records
  scat_fun(rare$total.rare.50, 
           rare$total.rare.40, 
           "50-record rarefaction", 
           "40-record rarefaction", 
           "40-50 records.jpeg")

# 45 records
  scat_fun(rare$total.rare.50, 
           rare$total.rare.45, 
           "50-record rarefaction", 
           "45-record rarefaction", 
           "45-50 records.jpeg")


