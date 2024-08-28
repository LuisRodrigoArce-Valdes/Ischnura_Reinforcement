# Script to make glm modeling per reproductive barriers. We used the binomial distribution with the number of eggs as weight for the fertility barrier following:
# For the :
# https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(MuMIn) # <- To automatize 
library(DHARMa) # <- To validate the best selected model.

# Creating directory to save DHARMA plots
dir.create("../results/DHARMA", showWarnings = F)

# Specifying DHARMA plots parameters (in cm)
w <- 30 #width
h <- 15 #height
r <- 600 #resolution

# Using this script we will make GLMs to test statistical differences in reproductive barriers between groups.
# Loading tidyed barriers files
rank <- "AICc" #BIC AICc

for(p in c("01_Prezygotics","02_Postzygotics")){
sink(paste0("../results/",p,"_Stats_",rank,".txt"), split = T)
print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
print(paste0("All models are ranked by: ", rank))
print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
load(paste0("../results/",p,".Rdata"))

# Setting up data ####

# Converting populations into intrapopulation vs interpopulation variables (this categorization wont be used in postzygotic barriers, ie, dont ming the warning)
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

# Binomials ####
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
  formula <- as.formula("Success ~ Ecology + Cross + Geography + (Ecology * Cross)")
} else {
  formula <- as.formula("Success ~ Ecology + Cross + (Ecology * Cross)")
}

# Modeling glms for binomial barriers
for(i in names(Binomials)){
  print("######################")
  print(i)
  print(formula)
  sub.df <- Binomials[[i]]
  glms <- glm(formula, data = sub.df, family = "binomial", na.action = "na.fail")
  # Dredging glms
  print(dredged <- dredge(glms, rank = rank))
  # Evaluating the best model
  glm1 <- eval(getCall(dredged, 1))
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Plotting the DHARMA plots to check for model fit
  png(paste0("../results/DHARMA/",p,"_",i,".png"), width = w, height = h, res = r, units = "cm")
  plot(simulateResiduals(fittedModel = glms))
  dev.off()
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
}

# Modeling non binomial variables ####
for(i in names(tidied)[4:5]){
  colnames(tidied[[i]])[4] <- "Success"
}

# Fecundity ####
for(i in names(tidied)[4]){
  print("######################")
  print(i)
  print(formula)
  sub.df <- tidied[[i]]
  sub.df$Success <- as.integer(round(sub.df$Success, digits = 0)) # Rounding to integers
  sub.df$Success[sub.df$Success==0] <- 1 # Converting 0s to 1s (since we have excluded non ovipositing females and poisson distributions needs positive values)
  
  # ¡NEW EDITIONS AFTER TESTING WITH DHARMA! ####
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  fam <- ifelse(p=="01_Prezygotics","gaussian","Gamma") #"!!!! gaussian worked better for prezygotics and Gamma for postzygotics"
  print(paste0("family for ", p, ": ", fam))
  glms <- glm(formula, data = sub.df, family = fam, na.action = "na.fail")
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Dredging glms
  print(dredged <- dredge(glms, rank = rank))
  # Evaluating the best model
  glm1 <- eval(getCall(dredged, 1))
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Plotting the DHARMA plots to check for model fit
  png(paste0("../results/DHARMA/",p,"_",i,".png"), width = w, height = h, res = r, units = "cm")
  plot(simulateResiduals(fittedModel = glms))
  dev.off()
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
}

# Fertility ####
i <- names(tidied)[5]
for(i in names(tidied)[5]){
  print("######################")
  print(i)
  print(formula)
  sub.df <- tidied[[i]]
  # rounding up success cases to avoid warnings (due to excel rounding of decimals)
  sub.df$Success <- round(sub.df$Success * sub.df$Fertility_N, digits = 0)
  # adding one egg to those that didnt had a single fertile egg and had a lot of eggs (becuase this is affecting statistical results when all eggs are unfertil)
  sub.df[sub.df$Ecology=="Allopatry" & sub.df$Cross=="hybridXgraellsii" & sub.df$Fertility_N>20,"Success"] <- 1
  sub.df$Success[sub.df$Success==0 & sub.df$Fertility_N > 50] <- 1
  
  # ¡NEW EDITIONS AFTER TESTING WITH DHARMA! ####
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Converting to 1 and 0s long table as the other binomial barriers.
  # Estimating unsucessful events (unfertile eggs)
  sub.df$Unsuccess <- sub.df$Fertility_N - sub.df$Success
  sub.df <- sub.df[,-5]
  # Adding female code
  sub.df$female <- paste0("Female_", 1:nrow(sub.df))
  # Tidying
  sub.df <- gather(sub.df, "Success","Fertile", 4:5)
  # Repeating rows (to transform it into 0 and 1 per female)
  sub.df <- sub.df[rep(row.names(sub.df), sub.df$Fertile),-6]
  # Converting to 1 and 0s
  sub.df$Success <- ifelse(sub.df$Success=="Success",1,0)
  # Modeling
  glms <- glm(formula, data = sub.df, family = "binomial", na.action = "na.fail")
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Dredging glms
  print(dredged <- dredge(glms, rank = rank))
  # Evaluating the best model
  glm1 <- eval(getCall(dredged, 1))
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Plotting the DHARMA plots to check for model fit
  png(paste0("../results/DHARMA/",p,"_",i,".png"), width = w, height = h, res = r, units = "cm")
  plot(simulateResiduals(fittedModel = glms))
  dev.off()
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
}
sink()
}