# Script to make glm modeling per reproductive barriers. We used the binomial distribution with the number of eggs as weight for the fertility barrier following:
# For the :
# https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(MuMIn) # <- To automatize aic tables 
library(DHARMa) # <- To validate the best selected model
library(emmeans) # <- For posthoc tests

# Creating directory to save DHARMA plots
dir.create("../results/DHARMA", showWarnings = F)

# Specifying DHARMA plots parameters (in cm)
w <- 30 #width
h <- 15 #height
r <- 600 #resolution

# Using this script we will make GLMs to test statistical differences in reproductive barriers between groups.
# Loading tidyed barriers files
rank <- "AICc" #BIC AICc

for(p in c("01_Prezygotics", "02_Postzygotics")){
sink(paste0("../results/",p,"_Stats_",rank,".txt"), split = T)
print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
print(paste0("All models are ranked by: ", rank))
print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
load(paste0("../results/",p,".Rdata"))

# Binomials ####
# Extracting binomials barriers
Binomials <- list()
for(i in names(tidied)[1:3]){
  Binomials[[i]] <- tidied[[i]]
  colnames(Binomials[[i]])[3] <- "Success"
  Binomials[[i]]$Success <- grepl("Succesfull",Binomials[[i]]$Success)
  Binomials[[i]]$Success <- ifelse(Binomials[[i]]$Success==T,1,0)
}

# Creating prezygotics and postzygotics formulas
if(p == "01_Prezygotics"){
  formula <- as.formula("Success ~ Ecology + Cross + (Ecology * Cross)")
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
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Plotting the DHARMA plots to check for model fit
  png(paste0("../results/DHARMA/",p,"_",i,".png"), width = w, height = h, res = r, units = "cm")
  plot(simulateResiduals(fittedModel = glms))
  dev.off()
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Dredging glms
  print(dredged <- dredge(glms, rank = rank))
  
  # Posthoc analyses with the full model
  print("&&&&&&&&&&&&&&&&&&&&&")
  print("Posthoc test")
  print("Ecology ~ Cross")
  em <- emmeans(glms, by = "Cross", specs = "Ecology")
  print(contrast(em, method = "pairwise", adjust = "Tukey"))
  print("Cross ~ Ecology")
  em <- emmeans(glms, by = "Ecology", specs = "Cross")
  print(contrast(em, method = "pairwise", adjust = "Tukey"))
}

# Modeling non binomial variables ####
for(i in names(tidied)[4:5]){
  colnames(tidied[[i]])[3] <- "Success"
}

# Fecundity ####
i <- names(tidied)[4]
  print("######################")
  print(i)
  print(formula)
  sub.df <- tidied[[i]]
  sub.df$Success <- as.integer(round(sub.df$Success, digits = 0)) # Rounding to integers
  sub.df$Success[sub.df$Success==0] <- 1 # Converting 0s to 1s (since we have excluded non ovipositing females and poisson distributions needs positive values)
  
  # ¡NEW EDITIONS AFTER TESTING WITH DHARMA! ####
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  fam <- ifelse(p=="01_Prezygotics","Gamma","Gamma") #"!!!! gaussian worked better for prezygotics and Gamma for postzygotics"
  print(paste0("family for ", p, ": ", fam))
  glms <- glm(formula, data = sub.df, family = fam, na.action = "na.fail")
  
  png(paste0("../results/DHARMA/",p,"_",i,".png"), width = w, height = h, res = r, units = "cm")
  plot(simulateResiduals(fittedModel = glms))
  dev.off()
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Dredging glms
  print(dredged <- dredge(glms, rank = rank))
  
  # Posthoc analyses with the full model
  print("&&&&&&&&&&&&&&&&&&&&&")
  print("Posthoc test")
  print("Ecology ~ Cross")
  em <- emmeans(glms, by = "Cross", specs = "Ecology")
  print(contrast(em, method = "pairwise", adjust = "Tukey"))
  print("Cross ~ Ecology")
  em <- emmeans(glms, by = "Ecology", specs = "Cross")
  print(contrast(em, method = "pairwise", adjust = "Tukey"))
  
# Fertility ####
i <- names(tidied)[5]
  print("######################")
  print(i)
  print(formula)
  sub.df <- tidied[[i]]
  # rounding up success cases to avoid warnings (due to excel rounding of decimals)
  sub.df$Success <- round(sub.df$Success * sub.df$Fertility_N, digits = 0)
  # adding one egg when a high number of infertile eggs was seen (without it GLMs behave weridly):
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
  sub.df <- gather(sub.df, "Success","Fertile", 3:4)
  # Repeating rows (to transform it into 0 and 1 per female)
  sub.df <- sub.df[rep(row.names(sub.df), sub.df$Fertile),-5]
  # Converting to 1 and 0s
  sub.df$Success <- ifelse(sub.df$Success=="Success",1,0)
  # Modeling
  glms <- glm(formula, data = sub.df, family = "binomial", na.action = "na.fail")
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Plotting the DHARMA plots to check for model fit
  png(paste0("../results/DHARMA/",p,"_",i,".png"), width = w, height = h, res = r, units = "cm")
  plot(simulateResiduals(fittedModel = glms))
  dev.off()
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  # Dredging glms
  print(dredged <- dredge(glms, rank = rank))
  
  # Posthoc analyses with the full model
  print("&&&&&&&&&&&&&&&&&&&&&")
  print("Posthoc test")
  print("Ecology ~ Cross")
  em <- emmeans(glms, by = "Cross", specs = "Ecology")
  print(contrast(em, method = "pairwise", adjust = "Tukey"))
  print("Cross ~ Ecology")
  em <- emmeans(glms, by = "Ecology", specs = "Cross")
  print(contrast(em, method = "pairwise", adjust = "Tukey"))

sink()
}