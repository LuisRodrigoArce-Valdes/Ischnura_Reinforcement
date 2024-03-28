# Script to make glm modeling per reproductive barriers. We used the binomial distribution with the number of eggs as weight for the fertility barrier following:
# For the :
# https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(MuMIn)
library(DHARMa) # <- To validate the best selected model.

# Creating directory to save DHARMA plots
dir.create("../results/DHARMA", showWarnings = F)

# Specifying DHARMA plots parameters (in cm)
wi <- 30 #width
h <- 15 #height
r <- 600 #resolution

# Using this script we will make GLMs to test statistical differences in reproductive barriers between groups.
# Loading tidyed barriers files
rank <- "AICc" #BIC AICc
p <- "01_Prezygotics"

sink(paste0("../results/",p,"_Asymmetries_",rank,".txt"), split = T)
print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
print(paste0("All models are ranked by: ", rank))
print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
load(paste0("../../01_Absolutes/results/",p,".Rdata"))
  
# Setting up data ####
  
# Cleaning populations names
  for(i in names(tidied)){
    tidied[[i]]$Population <- gsub(" .*$","",tidied[[i]]$Population)
    tidied[[i]] <- tidied[[i]][tidied[[i]]$Cross=="elegansXgraellsii" | tidied[[i]]$Cross=="graellsiiXelegans",]
  }

  print(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
  print("GLOBAL TESTS")
  print(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
  
  # Saving full data
  full <- tidied
  
  # Global tests ####
  for(eco in unique(tidied$mechanicals$Ecology)){
    print("")
    print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    print(eco)
    print("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    
    # Selecting ecology
    for(i in names(full)){
      tidied[[i]] <- full[[i]][full[[i]]$Ecology==eco,]
    }
    
  # Binomials ###
  # Extracting binomials barriers
  Binomials <- list()
  for(i in names(tidied)[1:3]){
    Binomials[[i]] <- tidied[[i]]
    colnames(Binomials[[i]])[4] <- "Success"
    Binomials[[i]]$Success <- grepl("Succesfull",Binomials[[i]]$Success)
    Binomials[[i]]$Success <- ifelse(Binomials[[i]]$Success==T,1,0)
  }
  
  # Creating prezygotics and postzygotics formulas
  formula <- as.formula("Success ~ Cross")
  
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
    png(paste0("../results/DHARMA/",eco,"_",i,".png"), width = wi, height = h, res = r, units = "cm")
    plot(simulateResiduals(fittedModel = glm1))
    dev.off()
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    print("The results of the best model are: ")
    print(summary(glm1))
    # Getting the intercepts
    print("The intercept levels are: ")
    for(q in names(glm1$xlevels)){
      print(paste0(q,":"))
      print(glms$xlevels[[q]][1])
    }
  }
  
  # Modeling non binomial variables 
  for(i in names(tidied)[4:5]){
    colnames(tidied[[i]])[4] <- "Success"
  }
  
  # Fecundity 
  for(i in names(tidied)[4]){
    print("######################")
    print(i)
    print(formula)
    sub.df <- tidied[[i]]
    sub.df$Success <- as.integer(round(sub.df$Success, digits = 0)) # Rounding to integers
    sub.df$Success[sub.df$Success==0] <- 1 # Converting 0s to 1s (since we have excluded non ovipositing females and poisson distributions needs positive values)
    glms <- glm(formula, data = sub.df, family = "Gamma", na.action = "na.fail")
    # Dredging glms
    print(dredged <- dredge(glms, rank = rank))
    # Evaluating the best model
    glm1 <- eval(getCall(dredged, 1))
    
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    # Plotting the DHARMA plots to check for model fit
    png(paste0("../results/DHARMA/",eco,"_",i,".png"), width = wi, height = h, res = r, units = "cm")
    plot(simulateResiduals(fittedModel = glm1))
    dev.off()
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    print("The results of the best model are: ")
    print(summary(glm1))
    # Getting the intercepts
    print("The intercept levels are: ")
    for(q in names(glm1$xlevels)){
      print(paste0(q,":"))
      print(glms$xlevels[[q]][1])
    }
  }
  
  # Fertility 
  for(i in names(tidied)[5]){
    print("######################")
    print(i)
    print(formula)
    sub.df <- tidied[[i]]
    # rounding up success cases to avoid warnings (due to excel rounding of decimals)
    sub.df$Success <- round(sub.df$Success * sub.df$Fertility_N, digits = 0)
    # adding one egg to those that didnt had a single fertile egg and had a lot of eggs (becuase this is affecting statistical results when all eggs are unfertil)
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
    png(paste0("../results/DHARMA/",eco,"_",i,".png"), width = wi, height = h, res = r, units = "cm")
    plot(simulateResiduals(fittedModel = glm1))
    dev.off()
    # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    print("The results of the best model are: ")
    print(summary(glm1))
    # Getting the intercepts
    print("The intercept levels are: ")
    for(q in names(glm1$xlevels)){
      print(paste0(q,":"))
      print(glms$xlevels[[q]][1])
    }
  }
}
  
# Per population tests ####
rm(list = setdiff(ls(),c("full","rank","p","wi","h","r","eco")))

# Reading selected populations
asymmetries <- read.csv("../data/Absolute_Isolation.csv")
asymmetries %>%
  filter(Asymmetries == "Y") -> asymmetries

# Tyding data
for(i in names(full)){
  # Filtering data to only include this localities
  full[[i]] <- full[[i]][full[[i]]$Population %in% asymmetries$Population,]
  
  # Unifying names
  full[[i]][full[[i]]$Population=="CachadasXBelgium","Population"] <- "BelgiumXCachadas"
  full[[i]][full[[i]]$Population=="LouroXCorrubedo","Population"] <- "CorrubedoXLouro"
  full[[i]][full[[i]]$Population=="LouroXLanzada","Population"] <- "LanzadaXLouro"
  full[[i]][full[[i]]$Population=="LaxeXCachadas","Population"] <- "CachadasXLaxe"
}

# Binomials ###
# Extracting binomials barriers
Binomials <- list()
for(i in names(full)[1:3]){
  Binomials[[i]] <- full[[i]]
  colnames(Binomials[[i]])[4] <- "Success"
  Binomials[[i]]$Success <- grepl("Succesfull",Binomials[[i]]$Success)
  Binomials[[i]]$Success <- ifelse(Binomials[[i]]$Success==T,1,0)
}

# Creating prezygotics and postzygotics formulas
formula <- as.formula("Success ~ Cross")

# Modeling glms for binomial barriers
for(i in names(Binomials)){
  print("######################")
  print(i)
  print(formula)
  for(w in unique(Binomials[[i]]$Population)){
  print("////////////////////")
  print(w)
  print("///////////////////")
  sub.df <- Binomials[[i]][Binomials[[i]]$Population==w,]
  if(length(unique(sub.df$Cross))>1){
  glms <- glm(formula, data = sub.df, family = "binomial", na.action = "na.fail")
  # Dredging glms
  print(dredged <- dredge(glms, rank = rank))
  # Evaluating the best model
  glm1 <- eval(getCall(dredged, 1))
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Plotting the DHARMA plots to check for model fit
  png(paste0("../results/DHARMA/",eco,"_",i,"_",w,".png"), width = wi, height = h, res = r, units = "cm")
  plot(simulateResiduals(fittedModel = glm1))
  dev.off()
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  print("The results of the best model are: ")
  print(summary(glm1))
  # Getting the intercepts
  print("The intercept levels are: ")
  for(q in names(glm1$xlevels)){
    print(paste0(q,":"))
    print(glms$xlevels[[q]][1])
  }
  } else {print("Only one cross direction")}
}
}

# Modeling non binomial variables 
for(i in names(full)[4:5]){
  colnames(full[[i]])[4] <- "Success"
}

# Fecundity 
for(i in names(full)[4]){
  print("######################")
  print(i)
  print(formula)
  for(w in unique(full[[4]]$Population)){
    print("////////////////////")
    print(w)
    print("///////////////////")
    sub.df <- full[[4]][full[[4]]$Population==w,]
    if(length(unique(sub.df$Cross))>1){
  sub.df$Success <- as.integer(round(sub.df$Success, digits = 0)) # Rounding to integers
  sub.df$Success[sub.df$Success==0] <- 1 # Converting 0s to 1s (since we have excluded non ovipositing females and poisson distributions needs positive values)
  glms <- glm(formula, data = sub.df, family = "Gamma", na.action = "na.fail")
  # Dredging glms
  print(dredged <- dredge(glms, rank = rank))
  # Evaluating the best model
  glm1 <- eval(getCall(dredged, 1))
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Plotting the DHARMA plots to check for model fit
  png(paste0("../results/DHARMA/",eco,"_",i,"_",w,".png"), width = wi, height = h, res = r, units = "cm")
  plot(simulateResiduals(fittedModel = glm1))
  dev.off()
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  print("The results of the best model are: ")
  print(summary(glm1))
  # Getting the intercepts
  print("The intercept levels are: ")
  for(q in names(glm1$xlevels)){
    print(paste0(q,":"))
    print(glms$xlevels[[q]][1])
  }
  } else {print("Only one cross direction")}
  }
  }

# Fertility 
for(i in names(full)[5]){
  print("######################")
  print(i)
  print(formula)
  for(w in unique(full[[5]]$Population)){
    print("////////////////////")
    print(w)
    print("///////////////////")
    sub.df <- full[[5]][full[[5]]$Population==w,]
    if(length(unique(sub.df$Cross))>1){
      # rounding up success cases to avoid warnings (due to excel rounding of decimals)
      sub.df$Success <- round(sub.df$Success * sub.df$Fertility_N, digits = 0)
      # adding one egg to those that didnt had a single fertile egg and had a lot of eggs (becuase this is affecting statistical results when all eggs are unfertil)
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
  png(paste0("../results/DHARMA/",eco,"_",i,"_",w,".png"), width = wi, height = h, res = r, units = "cm")
  plot(simulateResiduals(fittedModel = glm1))
  dev.off()
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  print("The results of the best model are: ")
  print(summary(glm1))
  # Getting the intercepts
  print("The intercept levels are: ")
  for(q in names(glm1$xlevels)){
    print(paste0(q,":"))
    print(glms$xlevels[[q]][1])
    }
    } else {print("Only one cross direction")}
  }
}

# Sinking
sink()
