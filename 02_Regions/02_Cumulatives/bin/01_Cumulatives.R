# Finally the plot of the big accumulative plots
rm(list = ls())
library(ggplot2)
library(tidyr)
library(rvg)
library(officer)
library(dplyr)
library(ggh4x)
library(scales)

# Figure options ####
# Text size in points
s <- 12
x <- 8

# Tidying data ####
cumulative <- read.csv(paste0("../data/Cumulatives",".csv"), header = T, encoding = "UTF-8")
colnames(cumulative) <- gsub(".Isolation","",colnames(cumulative))

# Tidying
cumulative %>%
  mutate(Cross = paste0(Male,"X",Female)) %>%
  gather("Barrier", "Isolation", 6:10) %>%
  mutate(Barrier = factor(Barrier, levels = unique(Barrier))) %>%
  mutate(Type = ifelse(Type=="Postzygotics","Postzygotics","Prezygotics")) %>%
  mutate(Type = factor(Type, levels = c("Prezygotics","Postzygotics"))) %>%  
  mutate(Cross = factor(Cross, levels = unique(Cross))) %>%
  select(!N) -> cumulative

# Prezygotics crosses ####
cumulative %>%
  filter(Type == "Prezygotics") %>%
  ggplot() +
  facet_wrap2(. ~ Cross, ncol = 2) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x=Barrier, y=Isolation, group = Ecology, color = Ecology)) +
  geom_point(aes(x=Barrier, y=Isolation, color = Ecology)) + 
  scale_y_continuous(labels = percent, n.breaks = 6) +
  scale_color_manual(values = c("#7570b3","#e7298a")) +
  theme_classic() +
  labs(y="Cumulative Reproductive Isolation") +
  theme(axis.title.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        text = element_text(family = "serif", size = s),
        axis.text.x = element_text(hjust = 1, angle = 10, size = x),
        legend.position = "none") -> p

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../../../00_BasePPTX/PNAS_Big_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = "../figures/01_Prezygotics.pptx")


# Postzygotics crosses ####
cumulative %>%
  filter(Type == "Postzygotics") %>%
  mutate(Cross = factor(as.vector(Cross), levels = c("ElegansXHybrid","GraellsiiXHybrid","HybridXHybrid","HybridXElegans","HybridXGraellsii"))) %>% 
  ggplot() +
  facet_wrap2(. ~ Cross, nrow = 2) +
  geom_hline(yintercept = 0) +
  geom_line(aes(x=Barrier, y=Isolation, group = Ecology, color = Ecology)) +
  geom_point(aes(x=Barrier, y=Isolation, color = Ecology)) + 
  scale_y_continuous(labels = percent, n.breaks = 6) +
  scale_color_manual(values = c("#7570b3","#e7298a")) +
  theme_classic() +
  labs(y="Cumulative Reproductive Isolation") +
  theme(axis.title.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        text = element_text(family = "serif", size = s),
        axis.text.x = element_text(hjust = 1, angle = 15, size = x),
        legend.position = "none") -> p

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../../../00_BasePPTX/PNAS_Big_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = "../figures/02_Postzygotics.pptx")
