rm(list = ls())

# Calling up libraries
library(tidyr)
library(ggplot2)
library(ggtext)
library(rvg)
library(here)
library(officer)

# Font size
s <- 8

# With this script we will make absolute isolation barriers bar plots
barriers <- read.csv("../data/Absolutes.csv", header = T, encoding = "UTF-8")

# Renaming Tactile Barrier
colnames(barriers)[8] <- "Mechanical-Tactile"

# Prezygotic barriers
postzygotics <- barriers[barriers$Generation!="F0",]

# Now for prezygotics
heterospecifics <- barriers[barriers$Generation=="F0",-c(4,5,6)]

# Pasting generation and type of cross
heterospecifics$Type <- paste0(heterospecifics$Generation, "-", heterospecifics$Cross)

# Removing more columns
heterospecifics <- heterospecifics[,-c(2,3)]

# Factorying type
heterospecifics$Type <- gsub("F0-","", heterospecifics$Type)
heterospecifics$Type <- factor(heterospecifics$Type, levels = unique(heterospecifics$Type))

# Tidying
heterospecifics <- gather(heterospecifics, "Barrier", "Isolation", 2:6)

# Factoring Barriers
heterospecifics$Barrier <- factor(heterospecifics$Barrier, levels = unique(heterospecifics$Barrier))

# Replacing 0 by a tiny value (to increase a little bit the bar)
heterospecifics[heterospecifics==0] <- 0.01

# Factoring
heterospecifics$Year <- factor(heterospecifics$Year, levels = c("Allopatric","2000-2001","2019-2020"))

# Plotting
p <- ggplot(heterospecifics[heterospecifics$Type=="E♂G♀" | heterospecifics$Type=="G♂E♀",]) +
  facet_wrap(. ~ Type, ncol = 2, scales = "fixed") +
  geom_col(aes(x=Barrier, y=Isolation, fill=Year, color=Year), position = position_dodge()) +
  scale_fill_manual(values = c('#fc8d62','#66c2a5','#8da0cb')) +
  scale_color_manual(values = c('#fc8d62','#66c2a5','#8da0cb')) +
  labs(y="Absolute Reproductive Isolation") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "gray"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s))

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../data/PNAS_Large_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = here::here("..","results","03_Absolutes_Prezygotics.pptx"))

# Heterospecifics crosses plots:
barriers <- postzygotics
heterospecifics <- barriers[barriers$Type=="Heterospecific",-c(4,5,6)]

# Postzygotic barriers
heterospecifics <- heterospecifics[heterospecifics$Generation!="F0",]

# Pasting generation and type of cross
heterospecifics$Type <- paste0(heterospecifics$Generation, "-", heterospecifics$Cross)

# Removing more columns
heterospecifics <- heterospecifics[,-c(2,3)]

# Factorying type
heterospecifics$Type <- factor(heterospecifics$Type, levels = c("F1-E♂H♀","F1-H♂E♀","F2-E♂H♀","F2-H♂E♀",
                                                                "F1-G♂H♀","F1-H♂G♀","F2-G♂H♀","F2-H♂G♀",
                                                                "F1-H♂H♀","F2-H♂H♀"))

# Tidying
heterospecifics <- gather(heterospecifics, "Barrier", "Isolation", 2:6)

# Factoring Barriers
heterospecifics$Barrier <- factor(heterospecifics$Barrier, levels = unique(heterospecifics$Barrier))

# Replacing 0 by a tiny value (to increase a little bit the bar)
heterospecifics[heterospecifics==0] <- 0.02

# Ordering by Type
heterospecifics <- heterospecifics[order(heterospecifics$Type),]

# Factoring
heterospecifics$Year <- factor(heterospecifics$Year, levels = c("Allopatric","2000-2001","2019-2020"))

# Plotting
p <- ggplot(heterospecifics) +
  geom_col(aes(x=Barrier, y=Isolation, fill=Year, color=Year),size=0.15, position = position_dodge()) +
  scale_fill_manual(values = c('#fc8d62','#66c2a5','#8da0cb')) +
  scale_color_manual(values = c('#fc8d62', '#66c2a5','#8da0cb')) +
  facet_wrap(. ~ Type, nrow = 4, dir = "v", scales = "fixed") +
  scale_y_continuous(limits = c(NA, 1)) +
  labs(y="Absolute Reproductive Isolation") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "gray"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        axis.text.x = element_text(hjust = 1, angle = 15))

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../data/PNAS_Big_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = here::here("..","results","04_Absolutes_Postzygotics.pptx"))