library(tidyverse)
df<-read.csv("wages.csv")

df <- df[complete.cases(df[,c(2,4)]),]

mods <-list()

modelsummary::datasummary_skim(df, output = "latex")

modelform<-formula(logwage~hgc+college+tenure+I(tenure^2)+age+married)

mods[['Complete']]<-lm(modelform, data = df[complete.cases(df[,1]),])

df.meanimpute<- df
df.meanimpute$logwage[is.na(df.meanimpute$logwage)]<- mean(df.meanimpute$logwage) 
mods[['Mean_Imputation']]<-lm(modelform, data = df.meanimpute)

df.comp<-df[complete.cases(df[,2:6]),]
df.na<-df.comp[is.na(df$logwage),]
df.nona<-df.comp[!is.na(df$logwage),]

tempmodel<-lm(logwage~., data=df.nona)
df.na$logwage<-predict(tempmodel, df.na)
df.lmimpute<-rbind(df.na, df.nona)
mods[['lm_Predicted']]<-lm(modelform,data = df.lmimpute)

df_mice<-mice::mice(df,m=5,printFlag = FALSE)
mods[['Mice']] <- with(df_mice$data,lm(logwage~hgc+college+tenure+I(tenure^2)+age+married))

modelsummary::modelsummary(mods, output = "latex")
