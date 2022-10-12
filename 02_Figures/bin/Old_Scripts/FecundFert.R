#Using this script we will make fecundity and fertility plots
rm(list = ls())

# Calling up libraries
library(ggplot2)
library(tidyr)
library(ggtext)

# Fecundity ####
# Reading files
fecundity <- read.csv("../data/Fecundities.csv", encoding = "UTF-8")
colnames(fecundity)[1] <- "Year"

# Adding up conspecific / heterospecific column
species <- vector()
for (i in 1:nrow(fecundity)){
  if (fecundity[i,"Cross"]=="E♂E♀" || fecundity[i,"Cross"]=="G♂G♀")
  {species <- c(species, "A")}
  else
  {species <- c(species, "B")}
}
fecundity$species <- species
rm(species)

# tidying
fecundity <- gather(fecundity, "Extra", "Fecundity", 5:(ncol(fecundity)-1))

# Removing extra column
fecundity <- fecundity[,-6]

# Removing empty rows
fecundity <- fecundity[complete.cases(fecundity),]

# For the conspecifics (A) replacing "F0-EXE" and "F0-GXG"
fecundity[fecundity$species=="A" & fecundity$Type=="F0-E♂E♀", "Type"] <- "*I. elegans*"
fecundity[fecundity$species=="A" & fecundity$Type=="F0-G♂G♀", "Type"] <- "*I. graellsii*"

# Factoring variables
fecundity$Year <- factor(fecundity$Year, levels = unique(fecundity$Year))
fecundity$Type <- factor(fecundity$Type, levels = unique(fecundity$Type))
fecundity$Cross <- factor(fecundity$Cross, levels = c("E♂E♀","G♂G♀","E♂G♀","G♂E♀","E♂H♀","H♂E♀","G♂H♀","H♂G♀","H♂H♀"))

# ggploting (only heterospecifics)
plotA <- ggplot(fecundity) +
  geom_boxplot(aes(x=Cross, y=Fecundity, color=Year), size=1.1) +
  facet_wrap(. ~ Generation, scales = "free_x") +
  scale_color_manual(values = c('#66c2a5','#8da0cb','#fc8d62')) +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 18),
        axis.title = element_blank(),
        text = element_text(family = "serif", size = 16),
        axis.text.x = element_markdown())

ggsave("../results/03_Fecundity.png", plotA, device = "png", width = 180*2, height = 180, units = "mm", dpi = 600, family = "Times")

# Fertility ####
rm(list = ls())

# Reading files
fertility <- read.csv("../data/Fertilities.csv", encoding = "UTF-8")
colnames(fertility)[1] <- "Year"

# Adding up conspecific / heterospecific column
species <- vector()
for (i in 1:nrow(fertility)){
  if (fertility[i,"Cross"]=="E♂E♀" || fertility[i,"Cross"]=="G♂G♀")
  {species <- c(species, "A")}
  else
  {species <- c(species, "B")}
}
fertility$species <- species
rm(species)

# tidying
fertility <- gather(fertility, "Extra", "Fertility", 5:(ncol(fertility)-1))

# Removing extra column
fertility <- fertility[,-6]

# Removing empty rows
fertility <- fertility[complete.cases(fertility),]

# For the conspecifics (A) replacing "F0-EXE" and "F0-GXG"
fertility[fertility$species=="A" & fertility$Type=="F0-E♂E♀", "Type"] <- "*I. elegans*"
fertility[fertility$species=="A" & fertility$Type=="F0-G♂G♀", "Type"] <- "*I. graellsii*"


# Factoring variables
fertility$Year <- factor(fertility$Year, levels = unique(fertility$Year))
fertility$Type <- factor(fertility$Type, levels = unique(fertility$Type))
fertility$Cross <- factor(fertility$Cross, levels = c("E♂E♀","G♂G♀","E♂G♀","G♂E♀","E♂H♀","H♂E♀","G♂H♀","H♂G♀","H♂H♀"))


# ggploting
plotA <- ggplot(fertility) +
  geom_boxplot(aes(x=Cross, y=Fertility, color=Year), size=1.1) +
  facet_wrap(. ~ Generation, scales = "free_x") +
  scale_color_manual(values = c('#66c2a5','#8da0cb','#fc8d62')) +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 18),
        axis.title = element_blank(),
        text = element_text(family = "serif", size = 16),
        axis.text.x = element_markdown())

ggsave("../results/04_Fertility.png", plotA, device = "png", width = 180*2, height = 180, units = "mm", dpi = 600, family = "Times")
