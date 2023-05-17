# Script to make glm modeling per reproductive barriers. We used the binomial distribution with the number of eggs as weight for the fertility barrier following:
# For the :
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
sink(paste0("../results/",p,"_Stats.txt"), split = T)
load(paste0("../results/",p,".Rdata"))

# Converting populations into intrapopulation vs interpopulation variables
for(i in names(tidied)){
  tidied[[i]]$Population <- gsub(" .*$","",tidied[[i]]$Population)
  tidied[[i]] <- separate(tidied[[i]], col = Population, into = c("Pop1","Pop2"), sep = "X", remove = T)
  tidied[[i]]$Geography <- ifelse(tidied[[i]]$Pop1==tidied[[i]]$Pop2,"Intrapopulation","Interpopulation")
  tidied[[i]] <- tidied[[i]][,-c(3,4)]
  if(i == "fertility"){
    tidied[[i]] <- tidied[[i]][,c(1,2,5,3,4)]
  } else {
    tidied[[i]] <- tidied[[i]][,c(1,2,4,3)]
  }
}

# Extracting binomials barriers
Binomials <- list()
for(i in names(tidied)[1:3]){
  Binomials[[i]] <- tidied[[i]]
  colnames(Binomials[[i]])[4] <- "Success"
  Binomials[[i]]$Success <- grepl("Succesfull",Binomials[[i]]$Success)
  Binomials[[i]]$Success <- ifelse(Binomials[[i]]$Success==T,1,0)
}

# Creating prezygotics and postzygotics formulas
if(p == "01_Prezygotics"){
  formula <- as.formula("Success ~ Ecology + Cross + Geography")
} else {
  formula <- as.formula("Success ~ Ecology + Cross")
}

# Modeling glms for binomial barriers
for(i in names(Binomials)){
  print("")
  print("######################")
  print(i)
  print(formula)
  sub.df <- Binomials[[i]]
  glms <- glm(formula, data = sub.df, family = "binomial", na.action = "na.fail")
  print(dredge(glms))
  
  # Now we will do the posthoc analyses of differences between crosses
  print("")
  print("POST HOC ANALYSES PER TYPE OF CROSS:")
  print("")
  
  for(n in unique(sub.df$Cross)){
     print("%%%%%")
     print(paste0("Intercept: ", n))
     # Selecting reference level
     sub.df$Cross <- relevel(factor(sub.df$Cross, ordered = F), ref = n)
     glms <- glm(Success ~ Cross, data = sub.df, family = "binomial", na.action = "na.fail")
     print(summary(glms))
  }
}

# Modeling non binomial variables
for(i in names(tidied)[4:5]){
  colnames(tidied[[i]])[4] <- "Param"
}

# Creating prezygotics and postzygotics formulas
if(p == "01_Prezygotics"){
  formula <- as.formula("Param ~ Ecology + Cross + Geography")
} else {
  formula <- as.formula("Param ~ Ecology + Cross")
}

# Fecundity
print("")
print("######################")
print("Fecundity")
print(formula)
sub.df <- tidied$fecundity
sub.df$Param <- as.integer(round(sub.df$Param, digits = 0)) # Rounding to integers
glms <- glm(formula, data = sub.df, family = "poisson", na.action = "na.fail")
print(dredge(glms))

# Now we will do the posthoc analyses of differences between crosses
print("")
print("POST HOC ANALYSES PER TYPE OF CROSS:")
print("")

for(n in unique(sub.df$Cross)){
  print("%%%%%")
  print(paste0("Intercept: ", n))
  # Selecting reference level
  sub.df$Cross <- relevel(factor(sub.df$Cross, ordered = F), ref = n)
  glms <- glm(Param ~ Cross, data = sub.df, family = "poisson", na.action = "na.fail")
  print(summary(glms))
}

# Fertility
print("")
print("######################")
print("Fertility")
print(formula)
sub.df <- tidied$fertility
glms <- glm(formula, data = sub.df, weights = sub.df$Fertility_N, family = "binomial", na.action = "na.fail")
print(dredge(glms))

# Now we will do the posthoc analyses of differences between crosses
print("")
print("POST HOC ANALYSES PER TYPE OF CROSS:")
print("")

for(n in unique(sub.df$Cross)){
  print("%%%%%")
  print(paste0("Intercept: ", n))
  # Selecting reference level
  sub.df$Cross <- relevel(factor(sub.df$Cross, ordered = F), ref = n)
  glms <- glm(Param ~ Cross, data = sub.df, weights = sub.df$Fertility_N, family = "binomial", na.action = "na.fail")
  print(summary(glms))
}
sink()
}