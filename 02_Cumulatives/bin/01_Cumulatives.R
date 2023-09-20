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
cumulative <- cumulative[,-1]
colnames(cumulative)[7] <- "Mechanical-Tactile"

# Removing selected rows with low sample sizes and strange behaviours
cumulative %>%
  filter(Include=="Y") %>%
  select(!Include) -> cumulative

cumulative %>%
  group_by(Ecology, Cross) %>%
  mutate(id=row_number()) %>%
  mutate(id=as.character(id)) %>%
  gather("Barrier", "Isolation", 6:10) %>%
  mutate(Barrier = factor(Barrier, levels = unique(Barrier))) %>%
  mutate(Ecology = factor(Ecology, levels = c("Allopatry", "Sympatry"))) %>%
  mutate(Population = paste0(Population," - ",N)) %>%
  select(!N) -> cumulative

# Prezygotics crosses ####
cumulative %>%
  filter(Type == "Conspecific crosses" | Type == "Heterospecific crosses") %>%
  mutate(Cross = factor(Cross, levels = c("E♂E♀","G♂G♀","E♂G♀","G♂E♀"), labels = c("EE","GG","EG","GE"))) %>%
  ggplot() +
  facet_grid2(rows=vars(Ecology), cols = vars(Cross)) +
  geom_line(aes(x=Barrier, y=Isolation, group=Population, color = id)) +
  geom_point(aes(x=Barrier, y=Isolation,color = id)) +
  scale_y_continuous(n.breaks = 5, limits = c(0,1), labels = percent) +
  theme_classic() +
  labs(y="Cumulative Reproductive Isolation") +
  theme(axis.title.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        text = element_text(family = "serif", size = s),
        axis.text.x = element_text(hjust = 1, angle = 20, size = x),
        legend.position = "none") -> p

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../../00_BasePPTX/PNAS_Large_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = "../figures/01_Prezygotics.pptx")

# Postzygotics barriers ####
cumulative %>%
  filter(Type == "Postzygotics") %>%
  mutate(Cross = factor(Cross, levels = c("E♂H♀","G♂H♀","H♂H♀","H♂E♀","H♂G♀"), labels = c("EH","GH","HH","HE","HG"))) %>%
  ggplot() +
  facet_grid2(rows=vars(Ecology), cols = vars(Cross)) +
  geom_line(aes(x=Barrier, y=Isolation, group=Population, color = id)) +
  geom_point(aes(x=Barrier, y=Isolation,color = id)) +
  scale_y_continuous(n.breaks = 5, limits = c(0,1), labels = percent) +
  theme_classic() +
  labs(y="Cumulative Reproductive Isolation") +
  theme(axis.title.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        text = element_text(family = "serif", size = s),
        axis.text.x = element_text(hjust = 1, angle = 20, size = x),
        legend.position = "none") -> p

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../../00_BasePPTX/PNAS_Large_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = "../figures/02_Postzygotics.pptx")