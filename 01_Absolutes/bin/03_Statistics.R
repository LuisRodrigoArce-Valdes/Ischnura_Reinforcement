# Solution:
# https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
rm(list = ls())
library(dplyr)
library(tidyr)
library(betareg)
library(ggplot2)
library(MuMIn)

# Using this script we will make GLMs to test statistical differences in reproductive barriers between groups.
# Loading tidyed barriers files
for(p in c("01_Prezygotics","02_Postzygotics")){
sink(paste0("../results/",p,"_Stats.txt"))
load(paste0("../results/",p,".Rdata"))

# Extracting binomials barriers
Binomials <- list()
for(i in names(tidied)[1:3]){
  Binomials[[i]] <- tidied[[i]]
  colnames(Binomials[[i]])[4] <- "Success"
  Binomials[[i]]$Success <- grepl("Succesfull",Binomials[[i]]$Success)
  Binomials[[i]]$Success <- ifelse(Binomials[[i]]$Success==T,1,0)
}

# Modeling glms for binomial barriers
for(i in names(Binomials)){
  print("")
  print("######################")
  print(i)
  sub.df <- Binomials[[i]]
  glms <- glm(Success ~ Ecology + Cross + Population, data = sub.df, family = "binomial", na.action = "na.fail")
  print(dredge(glms))
  
  # Now fitting glms per type of cross
  for(n in unique(Binomials[[i]]$Cross)){
    print("%%%%%")
    print(n)
    sub.df <- Binomials[[i]][Binomials[[i]]$Cross==n,]
    glms <- glm(Success ~ Ecology + Population, data = sub.df, family = "binomial", na.action = "na.fail")
    print(dredge(glms))
  }
}

# Extractiong non binomial variables
Gammas <- list()
for(i in names(tidied)[4:5]){
  Gammas[[i]] <- tidied[[i]]
  colnames(Gammas[[i]])[4] <- "Param"
}

# Modeling glms for non-binomial barriers using gamma
for(i in names(Gammas)){
  print("")
  print("######################")
  print(i)
  sub.df <- Gammas[[i]]
  glms <- glm(Param ~ Ecology + Cross + Population, data = sub.df, family = "gaussian", na.action = "na.fail")
  print(dredge(glms))
  
  # Now fitting glms per type of cross
  for(n in unique(Gammas[[i]]$Cross)){
    print("%%%%%")
    print(n)
    sub.df <- Gammas[[i]][Gammas[[i]]$Cross==n,]
    glms <- glm(Param ~ Ecology + Population, data = sub.df, family = "gaussian", na.action = "na.fail")
    print(dredge(glms))
  }
}
sink()
}