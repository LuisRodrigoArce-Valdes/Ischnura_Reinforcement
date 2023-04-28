# Solution:
# https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
rm(list = ls())
library(dplyr)
library(tidyr)
library(betareg)
library(ggplot2)

# Using this script we will make GLMs to test statistical differences in reproductive barriers between groups.

# Within 2019 ####
# Reading data
DB.2019 <- read.csv("../data/2019_R.csv")
colnames(DB.2019)[1] <- "Pair"

# Splitting group code into groups
DB.2019 %>%
  filter(Pair!="0") %>%
  separate(Group, into = c("Generation","Cross","Population"), sep = "-", remove = T) -> DB.2019

# Statistically tesing
sink("../results/01_Within2019.txt", split = T)
cat("WITHIN 2019 STATISTICAL TESTS\n")
cat("\n")

# EXG: LaxeXCachadas VS CachadasXLaxe ####
cat("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n")
cat("First we will test if there are differences between (LaxeXCachadas) and (LaxeXMontalvo), which are the two interspecific population crosses:\n")
DB.2019 %>%
  filter(Generation=="F0" & Cross == "elegansXgraellsii") %>%
  mutate(ClutchesWEggs = ClutchesWEggs > 0) %>%
  mutate(ClutchesWEggs = ifelse(ClutchesWEggs == T, 1, 0)) -> ForGLMs

cat("### Mechanical:\n")
GLM <- glm(Mechanical.Sucess ~ Population, data = ForGLMs, weights = Mechanical.N, family = "binomial")
summary(GLM)

cat("### Mechanical-Tactile:\n")
GLM <- glm(Mechanical.Tactile.Success ~ Population, data = ForGLMs, weights = Mechanical_Tactile.N, family = "binomial")
summary(GLM)

cat("### Oviposition:\n")
GLM <- glm(ClutchesWEggs ~ Population, data = ForGLMs, family = "binomial")
summary(GLM)

cat("### Fecundity:\n")
cat("Testing for normality:\n")
shapiro.test(ForGLMs[ForGLMs$Population=="LaxeXMontalvo","EggsPerClutch"])
shapiro.test(ForGLMs[ForGLMs$Population=="LaxeXCachadas","EggsPerClutch"])
cat("Only one group distributes normally, I will use the Poisson family:\n")
GLM <- glm(ClutchesWEggs ~ Population, data = ForGLMs, family = "poisson")
summary(GLM)

cat("### Fertility:\n")
GLM <- glm(Fertility ~ Population, data = ForGLMs, weights = Fertility_N, family = "binomial")
summary(GLM)
cat("STATISTICALLY SIGNIFICANT DIFFERENCES!\n")

sink()