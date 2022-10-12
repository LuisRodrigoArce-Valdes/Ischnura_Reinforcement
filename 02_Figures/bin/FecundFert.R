#Using this script we will make fecundity and fertility plots
rm(list = ls())

# Calling up libraries
library(ggplot2)
library(tidyr)
library(ggtext)
library(rvg)
library(here)
library(officer)

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

# Removing conspecifics
fecundity <- fecundity[fecundity$species=="B",]

# Removing empty rows
fecundity <- fecundity[complete.cases(fecundity),]

# Factoring variables
fecundity$Year <- factor(fecundity$Year, levels = c("Allopatric","2001","2019"))
fecundity$Type <- factor(fecundity$Type, levels = unique(fecundity$Type))
fecundity$Cross <- factor(fecundity$Cross, levels = c("E♂G♀","G♂E♀","E♂H♀","H♂E♀","G♂H♀","H♂G♀","H♂H♀"))

# ggploting (only heterospecifics)
p <- ggplot(fecundity) +
  geom_boxplot(aes(x=Cross, y=Fecundity, color=Year)) +
  facet_wrap(. ~ Generation, scales = "free_x") +
  scale_color_manual(values = c('#fc8d62','#66c2a5','#8da0cb')) +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        axis.title = element_blank(),
        text = element_text(family = "serif", size = 8),
        axis.text.x = element_markdown())

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../data/PNAS_Large_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = here::here("..","results","05_Fecundity.pptx"))

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

# Removing conspecifics
fertility <- fertility[fertility$species=="B",]

# Removing empty rows
fertility <- fertility[complete.cases(fertility),]

# Factoring variables
fertility$Year <- factor(fertility$Year, levels = c("Allopatric","2001","2019"))
fertility$Type <- factor(fertility$Type, levels = unique(fertility$Type))
fertility$Cross <- factor(fertility$Cross, levels = c("E♂G♀","G♂E♀","E♂H♀","H♂E♀","G♂H♀","H♂G♀","H♂H♀"))


# ggploting
p <- ggplot(fertility) +
  geom_boxplot(aes(x=Cross, y=Fertility, color=Year)) +
  facet_wrap(. ~ Generation, scales = "free_x") +
  scale_color_manual(values = c('#fc8d62','#66c2a5','#8da0cb')) +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        axis.title = element_blank(),
        text = element_text(family = "serif", size = 8),
        axis.text.x = element_markdown())

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../data/PNAS_Large_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = here::here("..","results","06_Fertility.pptx"))
