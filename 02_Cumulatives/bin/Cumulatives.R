# Finally the plot of the big accumulative plots
rm(list = ls())
library(ggplot2)
library(tidyr)
library(rvg)
library(officer)
library(dplyr)

# Figure options ####
# Creating color ramp palletes
PaletteSympatric <- colorRampPalette(c("skyblue1","navyblue"))
PaletteAllopatric <- colorRampPalette(c("lightpink1","red3"))

# Text size in points
s <- 12

# Tidying data ####
cumulative <- read.csv(paste0("../data/Cumulatives",".csv"), header = T, encoding = "UTF-8")
colnames(cumulative)[1] <- "Ecology"
colnames(cumulative)[7] <- "Mechanical-Tactile"

# Removing rows without mechanical measurements and tidying
cumulative %>%
  filter(N>0) %>%
  select(!X & !X.1) %>%
  gather("Barrier", "Isolation", 6:10) %>%
  mutate(Barrier = factor(Barrier, levels = unique(Barrier))) %>%
  mutate(Ecology = factor(Ecology, levels = c("Allopatry", "Sympatry"))) %>%
  mutate(Population = paste0(Population," - ",N)) %>%
  select(!N) -> cumulative

# Conspecifics crosses ####
cumulative %>%
  filter(Type == "Conspecific crosses") %>%
  mutate(Cross = factor(Cross, levels = c("E♂E♀","G♂G♀"))) -> Conspecifics

# Creating dataframe of colors
Conspecifics %>%
  select(Ecology, Population) %>%
  unique() %>%
  arrange(Ecology, Population) -> Colors

# Creating colors lists
colorsSympatric <- PaletteSympatric(nrow(Colors[Colors$Ecology=="Sympatry",]))
colorsAllopatric <- PaletteAllopatric(nrow(Colors[Colors$Ecology=="Allopatry",]))

# Merging colors to dataframe
Colors %>%
  mutate(colors = c(colorsSympatric, colorsAllopatric)) -> Colors
rm(colorsSympatric, colorsAllopatric)
colors <- setNames(Colors$colors, Colors$Population)
rm(Colors)

# Ploting
Conspecifics %>%
  ggplot() +
  facet_wrap(Cross ~ Ecology, ncol = 2, scales = "free") +
  geom_line(aes(x=Barrier, y=Isolation, color=Population, group=Population)) +
  geom_point(aes(x=Barrier, y=Isolation, color=Population)) +
  #scale_color_manual(values = colors) +
  scale_y_continuous(n.breaks = 5, limits = c(0,1)) +
  theme_classic() +
  labs(y="Cumulative Reproductive Isolation") +
  theme(axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        panel.grid.major.y = element_line(colour = "grey"),
        text = element_text(family = "serif", size = s),
        axis.text.x = element_text(hjust = 1, angle = 15),
        legend.text = element_text(size = s/2),
        legend.title = element_text(size = s/1.5),
        legend.position = "bottom") -> p

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../../00_BasePPTX/PNAS_Tall_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = paste0("../figures/01_Conspecifics",".pptx"))

# Heterospecific crosses ####
rm(Conspecifics)
cumulative %>%
  filter(Type == "Heterospecific crosses") %>%
  mutate(Cross = factor(Cross, levels = c("E♂G♀","G♂E♀"))) -> Heterospecifics

# Creating dataframe of colors
Heterospecifics %>%
  select(Ecology, Population) %>%
  unique() %>%
  arrange(Ecology, Population) -> Colors

# Creating colors lists
colorsSympatric <- PaletteSympatric(nrow(Colors[Colors$Ecology=="Sympatry",]))
colorsAllopatric <- PaletteAllopatric(nrow(Colors[Colors$Ecology=="Allopatry",]))

# Merging colors to dataframe
Colors %>%
  mutate(colors = c(colorsSympatric, colorsAllopatric)) -> Colors
rm(colorsSympatric, colorsAllopatric)
colors <- setNames(Colors$colors, Colors$Population)
rm(Colors)

# Ploting
Heterospecifics %>%
  ggplot() +
  facet_wrap(Cross ~ Ecology, ncol = 2, scales = "free") +
  geom_line(aes(x=Barrier, y=Isolation, color=Population, group=Population)) +
  geom_point(aes(x=Barrier, y=Isolation, color=Population)) +
  #scale_color_manual(values = colors) +
  scale_y_continuous(n.breaks = 5, limits = c(0,1)) +
  theme_classic() +
  labs(y="Cumulative Reproductive Isolation") +
  theme(axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        panel.grid.major.y = element_line(colour = "grey"),
        text = element_text(family = "serif", size = s),
        axis.text.x = element_text(hjust = 1, angle = 15),
        legend.text = element_text(size = s/2),
        legend.title = element_text(size = s/1.5),
        legend.position = "bottom") -> p

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../../00_BasePPTX/PNAS_Tall_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = paste0("../figures/02_Heterospecifics",".pptx"))

# Postzygotics barriers ####
rm(Heterospecifics)
cumulative %>%
  filter(Type == "Postzygotics") %>%
  mutate(Cross = factor(Cross, levels = c("E♂H♀","G♂H♀","H♂H♀","H♂E♀","H♂G♀"))) -> Postzygotics

# Ploting
s <- 10
Postzygotics %>%
  ggplot() +
  facet_grid(rows = vars(Ecology), cols = vars(Cross)) +
  geom_line(aes(x=Barrier, y=Isolation, color=Population, group=Population)) +
  geom_point(aes(x=Barrier, y=Isolation, color=Population)) +
  scale_y_continuous(n.breaks = 5, limits = c(0,1)) +
  theme_classic() +
  labs(y="Cumulative Reproductive Isolation") +
  theme(axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        panel.grid.major.y = element_line(colour = "grey"), 
        text = element_text(family = "serif", size = s),
        axis.text.x = element_text(hjust = 1, angle = 15, size = 6),
        legend.text = element_text(size = s/3),
        legend.title = element_text(size = s/2),
        legend.key.size = unit(1,"line"),
        legend.position = "bottom") -> p

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../../00_BasePPTX/PNAS_Big_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = paste0("../figures/03_Postzygotics",".pptx"))
