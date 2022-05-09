To recreate data and code:

The final project will be scored according to the following guidelines:

Download full Pokedex info available here: https://www.kaggle.com/datasets/cristobalmitchell/pokedex?resource=download

Add the competitive tier of each Pokemon, available here: https://www.smogon.com/dex/sm/pokemon/
(This was done manually)

Execute the following code to reproduce all results:

library(ggplot2)
library(forcats)

pokemon<-read.csv("pokemon.csv")

pokemon7$tier <- fct_reorder(pokemon7$tier, pokemon7$bst, mean)

ggplot(data=pokemon, mapping=aes(x=tier, y=bst)) + 
  stat_summary(fun.data=mean_sdl, geom="bar")

bstrelations<-lm(bst~ï..national_number+primary_type+secondary_type+weaksum, data=pokemon)

summary(bstrelations)
pokemon$secondary_type

pokemon7<-pokemon[-which(pokemon$gen=="VIII"),]
pokemon7

generateTeam <- function(tier, firstChoice){
  workingTier <- pokemon7[which(pokemon7$tier == tier),]
  workingTier <- workingTier[which(workingTier$english_name!=firstChoice),]
  
  pokemonList<-pokemon7[0,]
  pokemonList[1,]<-pokemon7[which(pokemon7$english_name==firstChoice),]

  typeResistances<-c(as.numeric(pokemonList[1,]$against_bug), as.numeric(pokemonList[1,]$against_normal), as.numeric(pokemonList[1,]$against_fire), as.numeric(pokemonList[1,]$against_water), as.numeric(pokemonList[1,]$against_electric), as.numeric(pokemonList[1,]$against_grass), 
                     as.numeric(pokemonList[1,]$against_ice), as.numeric(pokemonList[1,]$against_fighting), as.numeric(pokemonList[1,]$against_poison), as.numeric(pokemonList[1,]$against_ground), as.numeric(pokemonList[1,]$against_flying), as.numeric(pokemonList[1,]$against_psychic),
                     as.numeric(pokemonList[1,]$against_rock), as.numeric(pokemonList[1,]$against_ghost), as.numeric(pokemonList[1,]$against_dragon), as.numeric(pokemonList[1,]$against_dark), as.numeric(pokemonList[1,]$against_steel), as.numeric(pokemonList[1,]$against_fairy))
  
  dotprods<- function(x){
    x1<-as.numeric(x[36])*typeResistances[1]
    x2<-as.numeric(x[25])*typeResistances[2]
    x3<-as.numeric(x[26])*typeResistances[3]
    x4<-as.numeric(x[27])*typeResistances[4]
    x5<-as.numeric(x[28])*typeResistances[5]
    x6<-as.numeric(x[29])*typeResistances[6]
    x7<-as.numeric(x[30])*typeResistances[7]
    x8<-as.numeric(x[31])*typeResistances[8]
    x9<-as.numeric(x[32])*typeResistances[9]
    x10<-as.numeric(x[33])*typeResistances[10]
    x11<-as.numeric(x[34])*typeResistances[11]
    x12<-as.numeric(x[35])*typeResistances[12]
    x13<-as.numeric(x[37])*typeResistances[13]
    x14<-as.numeric(x[38])*typeResistances[14]
    x15<-as.numeric(x[39])*typeResistances[15]
    x16<-as.numeric(x[40])*typeResistances[16]
    x17<-as.numeric(x[41])*typeResistances[17]
    x18<-as.numeric(x[42])*typeResistances[18]
    
    return(x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18)
  }
  

  for (i in 2:6){

    pokemonList[i,]<- workingTier[which.min(apply(workingTier, 1, dotprods)),]
    
    workingTier<-workingTier[(which(workingTier$english_name != pokemonList[i,]$english_name)),]
    
    typeResistances<-typeResistances * c(pokemonList[i,]$against_bug, pokemonList[i,]$against_normal, pokemonList[i,]$against_fire, pokemonList[i,]$against_water, pokemonList[i,]$against_electric, pokemonList[i,]$against_grass,
                                         pokemonList[i,]$against_ice, pokemonList[i,]$against_fighting, pokemonList[i,]$against_poison, pokemonList[i,]$against_ground, pokemonList[i,]$against_flying, pokemonList[i,]$against_psychic,
                                         pokemonList[i,]$against_rock, pokemonList[i,]$against_ghost, pokemonList[i,]$against_dragon, pokemonList[i,]$against_dark, pokemonList[i,]$against_steel, pokemonList[i,]$against_fairy)
  }
  
  return(pokemonList)
}

generateTeam("OU", "Serperior")
