rm(list = ls())

# Calling up libraries
library(tidyr)
library(ggplot2)
library(ggtext)
library(rvg)
library(here)
library(officer)

# Font size
s <- 9

# With this script we will make absolute isolation barriers bar plots
barriers <- read.csv("../data/Reciprocal_Assimetries.csv", header = T, encoding = "UTF-8")
barriers <- barriers[!grepl("Postzygotic",barriers$Dataset),]
barriers$Barrier <- factor(barriers$Barrier, levels = rev(unique(barriers$Barrier)))

# Plotting
p <- ggplot(barriers) +
  facet_wrap(. ~ Dataset, ncol = 1, scales = "free_y") +
  geom_col(aes(x=Barrier, y=Difference.GE.EG, fill=Color), position = position_dodge()) +
  geom_hline(aes(yintercept=0)) +
  coord_flip() +
  scale_fill_manual(values = c('#984ea3','#a65628','#fb8072')) +
  scale_y_continuous(n.breaks = 5, limits = c(-1,1)) +
  labs(y="Reproductive isolation assymetry (GE - EG)") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "gray"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        text = element_text(family = "serif", size = s))

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../data/PNAS_Small_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = here::here("..","results","01_Asymetries.pptx"))