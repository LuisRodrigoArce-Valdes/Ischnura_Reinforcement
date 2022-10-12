rm(list = ls())
library(tidyr)
# Using this script we will make statistical tests!

# Saving output
sink(file = "../results/Statistics-Tests.txt", split = T)
print("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$")
print(" BINOMIAL TESTS ")

# Binomial ####
# Reading binomial data
binomial <- read.csv("../data/Hetero-Binomial.csv", header = T)

# Adding test conspecifics
binomial2 <- read.csv("../data/Conspecifics-Binomial.csv", header = T)
binomial <- rbind(binomial2, binomial)
rm(binomial2)

# Applying Fisher's test for each row using a for loop
for (i in 1:nrow(binomial)) {
  cross <- binomial[i,]
  print("######################################################################")
  print(cross[,"Barrier"])
  print(cross[,"Type"])
  mat <- data.frame(Success = t(cross[1,c(5,7,9)]), Fails=t(cross[1,c(6,8,10)]))
  colnames(mat) <- c("Success","Fails")
  row.names(mat) <- c("2001","2019","Allopatric")
  print(mat)
  mat <- mat[complete.cases(mat),]
  if(nrow(mat)>1) {
    print(fisher.test(mat))
    print("# POST-HOC Pariwise Fisher's exact tests: ")
    print(mat[1:2,])
    print(fisher.test(mat[1:2,]))
    if(nrow(mat)>2) {
      print(mat[c(1,3),])
      print(fisher.test(mat[c(1,3),]))
      print(mat[2:3,])
      print(fisher.test(mat[2:3,]))
    }
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
for (i in unique(fecundities$Type)){
  print("######################################################################")
  print("Fecundity")
  print(i)
  cross <- t(fecundities[fecundities$Type==i,5:ncol(fecundities)])
  colnames(cross) <- c("2001","2019", "Allopatric")
  row.names(cross) <- 1:nrow(cross)
  print(cross)
  # Removing NA columns
  cross <- cross[,colSums(is.na(cross))<nrow(cross)]
  cross <- as.data.frame(cross)
  if(ncol(cross)>1){
    print(kruskal.test(cross))
    cross <- gather(cross, "Year", "Value", 1:ncol(cross))
    cross <- cross[complete.cases(cross),]
    print(pairwise.wilcox.test(cross$Value, cross$Year, p.adjust.method = "none"))
  }
}
rm(cross)

# Tidying
fecundities <- fecundities%>%
  gather("Extra","Fecundity",5:ncol(fecundities))

# Removing EXTRA
fecundities <- fecundities[,-5]

# Removing NAs
fecundities <- fecundities[complete.cases(fecundities),]

print("")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("FECUNDITY SUMMARIES")

# Summaryzing for Paper results
tapply(fecundities$Fecundity, fecundities$Year, summary)
print("sd:")
tapply(fecundities$Fecundity, fecundities$Year, sd)
print("SE:")
tapply(fecundities$Fecundity, fecundities$Year, function(x) sd(x)/sqrt(length(x)))

# Fertilities ####
print("")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("FERTILITY")

# Statistically testing paired heterospecifics comparisons
for (i in unique(fertilities$Type)){
  print("######################################################################")
  print("Fertility")
  print(i)
  cross <- t(fertilities[fertilities$Type==i,5:ncol(fertilities)])
  colnames(cross) <- c("2001","2019", "Allopatric")
  row.names(cross) <- 1:nrow(cross)
  print(cross)
  # Removing NA columns
  cross <- cross[,colSums(is.na(cross))<nrow(cross)]
  cross <- as.data.frame(cross)
  if(ncol(cross)>1){
    print(kruskal.test(cross))
    cross <- gather(cross, "Year", "Value", 1:ncol(cross))
    cross <- cross[complete.cases(cross),]
    print(pairwise.wilcox.test(cross$Value, cross$Year, p.adjust.method = "none"))
  }
}
rm(cross)

# Tidying
fertilities <- fertilities%>%
  gather("Extra","Fertility",5:ncol(fertilities))

# Removing EXTRA
fertilities <- fertilities[,-5]

# Removing NAs
fertilities <- fertilities[complete.cases(fertilities),]

print("")
print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
print("FECUNDITY SUMMARIES")

# Summaryzing for Paper results
tapply(fertilities$Fertility, fertilities$Year, summary)
print("sd:")
tapply(fertilities$Fertility, fertilities$Year, sd)
print("SE:")
tapply(fertilities$Fertility, fertilities$Year, function(x) sd(x)/sqrt(length(x)))

# Saving output
sink()
