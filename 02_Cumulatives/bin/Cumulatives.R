# Finally the plot of the big accumulative plots
rm(list = ls())
library(ggplot2)
library(tidyr)
library(rvg)
library(officer)
library(dplyr)

# Figure options ####
# Creating color ramp palletes
Palette2019 <- colorRampPalette(c("skyblue1","navyblue"))
PaletteAllopatric <- colorRampPalette(c("lightpink1","red3"))

# Text size in points
s <- 12

# Reading data ####
cumulative <- read.csv("../data/Cumulatives.csv", header = T, encoding = "UTF-8")
colnames(cumulative)[1] <- "Ecology"
colnames(cumulative)[9] <- "Mechanical-Tactile"

# Removing one row
cumulative <- cumulative[cumulative$Population!="CachadasXMaraixDorx",]

# Tidying
cumulative %>% 
  gather("Barrier", "Isolation", 8:12) %>%
  mutate(Barrier = factor(Barrier, levels = unique(Barrier))) -> cumulative

# Selecting prezygotics
cumulative %>%
  filter(Cross=="G♂E♀" | Cross=="E♂G♀") %>%
  mutate(Cross = factor(Cross, levels = c("G♂E♀","E♂G♀"))) -> Prezygotics

# Creating dataframe of colors
Prezygotics %>%
  select(Database, Population) %>%
  unique() %>%
  arrange(Database, Population) -> Colors

# Creating colors lists
colors2019 <- Palette2019(length(Colors$Database[Colors$Database=="2019"]))
colorsAllopatric <- PaletteAllopatric(length(Colors$Database[Colors$Database=="Allopatric"]))

# Merging colors to dataframe
Colors %>%
  mutate(colors = c(colors2019, colorsAllopatric)) -> Colors
rm(colors2019, colorsAllopatric)
colors <- setNames(Colors$colors, Colors$Population)
rm(Colors)

# Filtering
Prezygotics %>%
  ggplot() +
  facet_wrap(Database ~ Cross, ncol = 2, scales = "free") +
  geom_line(aes(x=Barrier, y=Isolation, color=Population, group=Population)) +
  geom_point(aes(x=Barrier, y=Isolation, color=Population, shape=Database)) +
  scale_color_manual(values = colors) +
  scale_y_continuous(n.breaks = 5, limits = c(0,1)) +
  theme_classic() +
  labs(y="Cumulative Reproductive Isolation") +
  theme(axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        text = element_text(family = "serif", size = s),
        axis.text.x = element_text(hjust = 1, angle = 15),
        legend.text = element_text(size = s/2),
        legend.title = element_text(size = s/1.5),
        legend.position = "bottom") -> p

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../data/Base_PPTX/PNAS_Tall_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = "../results/01_Prezygotics.pptx")

# Filtering postzygotics
cumulative %>%
  filter(Generation!="F0") %>%
  mutate(Cross = factor(Cross, levels=c("E♂H♀","G♂H♀","H♂H♀","H♂E♀","H♂G♀"))) -> postzygotics

# Creating dataframe of colors
postzygotics %>%
  select(Database, Population) %>%
  unique() %>%
  arrange(Database, Population) -> Colors

# Creating colors lists
colors2019 <- Palette2019(length(Colors$Database[Colors$Database=="2019"]))
colorsAllopatric <- PaletteAllopatric(length(Colors$Database[Colors$Database=="Allopatric"]))

# Merging colors to dataframe
Colors %>%
  mutate(colors = c(colors2019, colorsAllopatric)) -> Colors
rm(colors2019, colorsAllopatric)
colors <- setNames(Colors$colors, Colors$Population)
rm(Colors)

postzygotics %>%
  ggplot() +
  facet_grid(rows=vars(Generation), cols = vars(Cross)) +
  geom_line(aes(x=Barrier, y=Isolation, color=Database, group=Population)) +
  geom_point(aes(x=Barrier, y=Isolation, color=Database)) +
  scale_color_manual(values = c('navyblue','red3')) + #'#66c2a5'
  #scale_color_manual(values = colors) +
  theme_classic() +
  labs(y="Cumulative Reproductive Isolation") +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        axis.text.x = element_text(hjust = 1, angle = 15),
        legend.position="bottom") -> p

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../data/Base_PPTX/PNAS_Large_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = "../results/02_Postzygotics.pptx")