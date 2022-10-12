rm(list = ls())
library(tidyr)
# Using this script we will make statistical tests!

# Saving output
sink(file = "../results/Statistics-Tests.txt", split = T)
print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
print(" BINOMIAL TESTS ")

# Binomial ####
# Reading binomial data
binomial <- read.csv("../data/Binomial.csv", header = T)

# Wont test conspecifics
# binomial2 <- read.csv("../data/Conspecifics-Binomial.csv", header = T)
# binomial <- rbind(binomial2, binomial)
# rm(binomial2)

# Applying Fisher's test for each row using a for loop
for (i in 1:nrow(binomial)) {
  cross <- binomial[i,]
  print("")
  print("######################################################################")
  print(cross[,"Barrier"])
  print(cross[,"Year"])
  mat <- data.frame(Success = t(cross[1,c(3,5)]), Fails=t(cross[1,c(4,6)]))
  colnames(mat) <- c("Success","Fails")
  row.names(mat) <- c("EG","GE")
  print(mat)
  mat <- mat[complete.cases(mat),]
  if(nrow(mat)>1) {
    print(fisher.test(mat))
  }
}

# Fecundities ####
rm(list = ls())

# Reading data
fecundities <- read.csv("../data/Fecundities.csv", header = T, encoding = "UTF-8")
fertilities <- read.csv("../data/Fertilities.csv", header = T, encoding = "UTF-8")

print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
print(" Kruskal / Wilcoxon TESTS ")
print("")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("FECUNDITY")

# Statistically testing paired heterospecifics comparisons
for (i in unique(fecundities$Year)){
  print("######################################################################")
  print("Fecundity")
  print(i)
  cross <- t(fecundities[fecundities$Year==i,5:ncol(fecundities)])
  colnames(cross) <- c("EG","GE")
  row.names(cross) <- 1:nrow(cross)
  print(cross)
  # Removing NA columns
  cross <- cross[,colSums(is.na(cross))<nrow(cross)]
  cross <- as.data.frame(cross)
  if(ncol(cross)>1){
    cross <- gather(cross, "Direction", "Value", 1:ncol(cross))
    cross <- cross[complete.cases(cross),]
    print(wilcox.test(Value ~ Direction, data = cross, exact = F))
  }
}
rm(cross)

# Fertilities ####
print("")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("FERTILITY")

# Statistically testing paired heterospecifics comparisons
for (i in unique(fertilities$Year)){
  print("######################################################################")
  print("Fertility")
  print(i)
  cross <- t(fertilities[fertilities$Year==i,5:ncol(fertilities)])
  colnames(cross) <- c("EG","GE")
  row.names(cross) <- 1:nrow(cross)
  print(cross)
  # Removing NA columns
  cross <- cross[,colSums(is.na(cross))<nrow(cross)]
  cross <- as.data.frame(cross)
  if(ncol(cross)>1){
    cross <- gather(cross, "Direction", "Value", 1:ncol(cross))
    cross <- cross[complete.cases(cross),]
    print(wilcox.test(Value ~ Direction, data = cross, exact = F))
  }
}
rm(cross)

# Sinking
sink()