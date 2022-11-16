

# library --------------------------------------------------------
  library(dplyr)
  library(data.table)
  
  rm(list = ls())

# data ------------------------------------------------------------------
# cleaned records from Poaceae script
  ala <- readRDS("data/ALA records/raw grass records.RDS") %>%
          rename(ala_stat = status) %>%
          ungroup() %>%
          mutate_all(as.character)
  glimpse(ala)

# sub tribe, immigration period (ip), endemism and height data
  tribes <- read.csv("data/tribes/9.1AVH_grass-SB-traits-KH.csv", strip.white = T) %>%
    rename(endemism = Endemic,
           sub_tribe = sub.family,
           ip = ageClass,
           hgt = culms) %>%
    dplyr::select(species, sub_tribe, endemism, ip, hgt) %>%
    mutate_all(as.character)
  glimpse(tribes)

  table(tribes$ip, exclude = NULL)

# tidying ALA data --------------------------------
# unnamed and hybrids: Spinifex x alterniflorus, Hymenachne x calamitosa, Lachnagrostis x punicea, & Lachnagrostis x contracta
  ala2 <- ala %>% 
          filter(!species %in% c("Spinifex alterniflorus", 
                                 "Hymenachne calamitosa", 
                                 "Lachnagrostis punicea", 
                                 "Lachnagrostis contracta"))
# remove Triodia genus
  ala3 <- filter(ala2, genus != "Triodia",
                 ala_stat != "nonnative")
  
# assign endemism, tribe (sub-family
# apc species list
  apc_spp <- ala3 %>% distinct(species, .keep_all = T)
  spp1 <- left_join(apc_spp, tribes, by = "species")
  spp1 <- spp1  %>% filter(ip != "uncertain")
  
# species that don't line up
  table(spp1$ala_stat, spp1$endemism, exclude = NULL)
  
#             "" endemic Endemic exotic EXOTIC   n   N <NA>
#    native   3      54     692      1      9 134   3   51
  miss_spp <- spp1 %>% filter(is.na(endemism))

# ALA = native, Sue = no label == native  
# exotic / EXOTIC == exotic; removed  
# n / N / == native  
# <NA> == exotic; removed
  nas <- c("Amphibromus vickeryae",
           "Aristida behriana",
           "Bothriochloa bladhii",
           "Brachypodium pinnatum",
           "Cenchrus advena",
           "Chrysopogon aciculatus",
           "Cymbopogon martinii",
           "Cynodon dactylon",
           "Cyrtococcum deltoideum",
           "Deschampsia chapmanii",
           "Deschampsia flexuosa",
           "Digitaria dolleryi",
           "Digitaria velutina",
           "Digitaria violascens",
           "Echinochloa colona",
           "Echinochloa telmatophila",
           "Eragrostis amabilis",
           "Eragrostis leptostachya",
           "Lolium persicum",
           "Milium effusum",
           "Mnesithea granularis",
           "Oryza nivara",
           "Paspalidium aversum",
           "Paspalidium distans",
           "Paspalum distichum",
           "Paspalum multinodum",
           "Phyllostachys pubescens",
           "Poa alpina",
           "Poa nemoralis",
           "Polypogon fugax",
           "Saccharum spontaneum",
           "Thaumastochloa heteromorpha", # hybrid
           "Urochloa distachya",
           "Urochloa reptans",
           "Urochloa subquadripara")
  
  spp2 <- (spp1$species %in% nas)
  
# note: I checked some <NA>s out and they are naturalised exotics 
  spp2 <- spp1 %>% 
          filter(endemism != "",
                 endemism != "exotic",
                 endemism != "EXOTIC",
                 !is.na(endemism)) %>%
          mutate(endemism = if_else(endemism == "n", "native", endemism)) %>%
          mutate(endemism = if_else(endemism == "N", "native", endemism)) %>%
          mutate(endemism = if_else(endemism == "Endemic", "endemic", endemism))
     
  table(spp2$endemism, exclude = NULL)   
    
  glimpse(spp2)
  
# Panicum species that are C3 species are being moved out of Paniceae subtribe
  table(spp2$sub_tribe, spp2$pp, exclude = NULL)  
  spp2[spp2$pp == "C3", "sub_tribe"] <- "C3"
  table(spp2$sub_tribe, spp2$pp, exclude = NULL)  
  
# merge sub tribe spp with records
  spp3 <- spp2 %>% dplyr::select(species, endemism, sub_tribe, ip, hgt)
  ala4 <- left_join(spp3, ala3, by = "species") %>% 
          dplyr::select(-ala_stat)
  glimpse(ala4)

# remove Aristida genus
  aris <- ala4 %>% 
          filter(genus == "Aristida") %>%
          distinct(species) %>%
          summarise(species = n())
  
  write.csv(aris, "results/csv/Aristida.csv", row.names = F)
  
  ala5 <- ala4 %>% filter(genus != "Aristida")
  
# # add in info for C3 recent and paelo hgt  
#   ala6 <- left_join(ala5, c3_pr, by = "species")
  
# checks
  table(ala5$endemism, exclude = F)
  table(ala5$pp, exclude = F)
  table(ala5$sub_tribe, exclude = F)
  table(ala5$hgt, exclude = F)
  table(ala5$ip, exclude = NULL)
# save    
  write.csv(ala5, "data/Ala records/master grass records.csv", row.names = F)
  
# -----------------------------------------------------------------------------------
  
  
  

