library(rvest)
library(tidyverse)

Pokemon_stats <- read_html("https://www.serebii.net/pokedex-sm/stat/all.shtml") %>%
  html_node(".dextable") %>%
  html_table(fill=TRUE)

colnames(Pokemon_stats) <- c("Nat Num", "Pic", "Type", "Name","Fake Col", "Abilities", "Base HP",
                             "Base Att", "Base Def", "Base S.Att", "Base S.Def", "Base Spd", "BST")

Pokemon_stats<-Pokemon_stats %>% mutate(`Fake Col`=NULL, Pic=NULL, Type = NULL)
  
Pokemon_stats<-Pokemon_stats[complete.cases(Pokemon_stats),]

Pokemon_stats

# API use:
WinnCounty<-fredr::fredr("ENU1720120510")
WinnCounty<-WinnCounty %>% mutate(series_id=NULL)
