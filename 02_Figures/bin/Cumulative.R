# Finally the plot of the big accumulative plots
rm(list = ls())
library(ggplot2)
library(tidyr)
library(rvg)
library(here)
library(officer)

# Figure options
# Text size in points
s <- 8

# Reading data
cumulative <- read.csv("../data/Cumulative.csv", header = T, encoding = "UTF-8")

# Tidying
cumulative <- gather(cumulative, "Barrier", "Isolation", 7:11)

# Changing second barrier name
cumulative[cumulative=="Tactile"] <- "Mechanical-Tactile"

# Factoring
cumulative$Barrier <- factor(cumulative$Barrier, levels = unique(cumulative$Barrier))
pre <- cumulative[cumulative$Cross=="E♂G♀" | cumulative$Cross=="G♂E♀",]
pre$Cross <- factor(pre$Cross, levels = c("G♂E♀","E♂G♀"))
pre$Barrier <- factor(pre$Barrier, levels = unique(pre$Barrier))


# Prezygotic plots
p <- ggplot(pre) +
          facet_wrap(. ~ Cross) +
          geom_line(aes(x=Barrier, y=Isolation, color=Year, group=Year)) +
          geom_point(aes(x=Barrier, y=Isolation, color=Year)) +
          scale_color_manual(values = c('#66c2a5','#8da0cb','#fc8d62')) +
          theme_classic() +
          labs(y="Cumulative Reproductive Isolation") +
          theme(axis.title.x = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(hjust = 0),
                text = element_text(family = "serif", size = s),
                axis.text.x = element_text(hjust = 1, angle = 15),
                legend.position = "none")

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../data/PNAS_Tiny_Image.pptx") %>%
# specify object and location of object (full size)
officer::ph_with(p_dml, ph_location_fullsize()) %>%
# export slide
print(target = here::here("..","results","01_Prezygotics.pptx"))

# Factoring
cumulative <- cumulative[cumulative$Generation!="F0",]
cumulative$Cross <- factor(cumulative$Cross, levels = c("E♂H♀","G♂H♀","H♂H♀","H♂E♀","H♂G♀"))

# Postzygotic plot
p <- ggplot(cumulative) +
  facet_grid(rows=vars(Generation), vars(Cross)) +
  geom_line(aes(x=Barrier, y=Isolation, color=Year, group=Year)) +
  geom_point(aes(x=Barrier, y=Isolation, color=Year)) +
  scale_color_manual(values = c('#66c2a5','#8da0cb','#fc8d62')) +
  theme_classic() +
  labs(y="Cumulative Reproductive Isolation") +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        axis.text.x = element_text(hjust = 1, angle = 15),
        legend.position="none")

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../data/PNAS_Large_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = here::here("..","results","02_Postzygotics.pptx"))