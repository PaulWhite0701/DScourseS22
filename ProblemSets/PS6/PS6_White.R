library(rvest)
library(tidyverse)
library(dplyr)

Pokemon_stats <- read_html("https://www.serebii.net/pokedex-sm/stat/all.shtml") %>%
  html_node(".dextable") %>%
  html_table(fill=TRUE)

colnames(Pokemon_stats) <- c("Nat Num", "Pic", "Type", "Name","Fake Col", "Abilities", "Base HP",
                             "Base Att", "Base Def", "Base S.Att", "Base S.Def", "Base Spd", "BST")

Pokemon_stats<-Pokemon_stats %>% mutate(`Fake Col`=NULL, Pic=NULL, Type = NULL)

n_occur <- data.frame(table(Pokemon_stats$`Nat Num`))
n_occur[n_occur$Freq > 1,]

Pokemon_stats<-Pokemon_stats %>% arrange(BST)
Pokemon_stats<- Pokemon_stats %>% distinct(`Nat Num`, .keep_all=TRUE)

Pokemon_stats<-Pokemon_stats[complete.cases(Pokemon_stats),]

Pokemon_stats <- Pokemon_stats %>% mutate(across(c("Base HP","Base Att", 
                                                   "Base Def", "Base S.Att", "Base S.Def", 
                                                   "Base Spd"), as.numeric))

Pokemon_stats$`Nat Num`<-readr::parse_number(Pokemon_stats$`Nat Num`)

StatRange <- vector(mode="integer", length = dim(Pokemon_stats)[1])
for (i in (1:dim(Pokemon_stats)[1])){
  StatRange[i] = (max(c(Pokemon_stats[[i,"Base HP"]],Pokemon_stats[[i,"Base Att"]],Pokemon_stats[[i,"Base Def"]],
                        Pokemon_stats[[i,"Base S.Att"]],Pokemon_stats[[i,"Base S.Def"]],Pokemon_stats[[i,"Base Spd"]]))- 
                    min(c(Pokemon_stats[[i,"Base HP"]],Pokemon_stats[[i,"Base Att"]],Pokemon_stats[[i,"Base Def"]],
                          Pokemon_stats[[i,"Base S.Att"]],Pokemon_stats[[i,"Base S.Def"]],Pokemon_stats[[i,"Base Spd"]])))
}

Pokemon_stats$StatRange = StatRange

ggplot(data = Pokemon_stats, aes(x = `Nat Num`, y=BST)) + geom_point() + geom_smooth()



ggplot(data = Pokemon_stats, aes(x = `Nat Num`, y=StatRange)) + geom_point() + geom_smooth()

ggplot(data = Pokemon_stats, aes(x = BST, y=StatRange)) + geom_point() + geom_smooth()
