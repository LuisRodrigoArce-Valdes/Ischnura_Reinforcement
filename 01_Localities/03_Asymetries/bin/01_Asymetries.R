rm(list = ls())

# Calling up libraries
library(tidyr)
library(ggplot2)
library(dplyr)
library(officer)
library(rvg)

# Figure options
# Text size in points
s <- 16

# Reading input file
absolutes <- read.csv("../data/Absolute_Isolation.csv")

# Filtering
absolutes %>%
  filter(Asymmetries == "Y") %>%
  select(!c("ID","Type","N","Asymmetries")) %>%
  mutate(Cross=factor(Cross, levels = c("G♂E♀","E♂G♀"))) -> absolutes

# Splitting into reciprocal crosses
asymmetries <- list()

absolutes %>%
  filter(Ecology=="Allopatry") -> asymmetries$allopatry

absolutes %>%
  filter(Population=="LouroXCorrubedo" | Population=="CorrubedoXLouro") -> asymmetries$sympatry1

absolutes %>%
  filter(Population=="LouroXLanzada" | Population=="LanzadaXLouro") -> asymmetries$sympatry2

absolutes %>%
  filter(Population=="LaxeXCachadas" | Population=="CachadasXLaxe") -> asymmetries$sympatry3

rm(absolutes)

# Sorting by the selected levels
for(i in names(asymmetries)){
  asymmetries[[i]] <- asymmetries[[i]][order(asymmetries[[i]]$Cross),]
  asymmetries[[i]]$Cross <- as.character(asymmetries[[i]]$Cross)
  row.names(asymmetries[[i]]) <- 1:nrow(asymmetries[[i]])
  asymmetries[[i]][3,1] <- asymmetries[[i]][1,1]
  asymmetries[[i]][3,2] <- "G♂E♀ - E♂G♀"
  asymmetries[[i]][3,3] <- paste0(asymmetries[[i]][1,3]," - ",asymmetries[[i]][2,3])
  asymmetries[[i]][3,c(4:8)] <- asymmetries[[i]][1,c(4:8)] - asymmetries[[i]][2,c(4:8)]
  asymmetries[[i]] <- asymmetries[[i]][-c(1,2),]
}

bind_rows(asymmetries, .id = "ID") -> asymmetries

# Estimating global assymetries
absolutes <- read.csv("../data/Absolute_Isolation.csv")

# Filtering
absolutes %>%
  filter(Type == "Heterospecific crosses") %>%
  select(!c("ID","Type","N","Asymmetries")) %>%
  mutate(Cross=factor(Cross, levels = c("G♂E♀","E♂G♀"))) -> absolutes

# Splitting allopatrics vs sympatrics
global <- list()

absolutes %>%
  filter(Ecology == "Allopatry") %>%
  group_by(Cross) %>%
  summarise_at(vars(Mechanical, Mechanical.Tactile, Oviposition, Fecundity, Fertility), mean, na.rm = T) %>%
  arrange(Cross) -> global$Allopatry

absolutes %>%
  filter(Ecology == "Sympatry") %>%
  group_by(Cross) %>%
  summarise_at(vars(Mechanical, Mechanical.Tactile, Oviposition, Fecundity, Fertility), mean, na.rm = T) %>%
  arrange(Cross) -> global$Sympatry
rm(absolutes)

# Substracting rows
# Sorting by the selected levels
for(i in names(global)){
  global[[i]] <- as.data.frame(global[[i]])
  global[[i]]$Cross <- as.character(global[[i]]$Cross)
  global[[i]][3,1] <- "G♂E♀ - E♂G♀"
  global[[i]][3,c(2:6)] <- global[[i]][1,c(2:6)] - global[[i]][2,c(2:6)]
  global[[i]] <- global[[i]][-c(1,2),]
}

bind_rows(global, .id = "Ecology") -> global

# Merging dataframes
global %>%
  mutate(Population = c("Average Allopatry","Average Sympatry")) %>%
  select(c(8,3:7)) -> global

asymmetries %>%
  mutate(Population = paste0(Ecology,": ", Population)) %>%
  select(4:9) -> asymmetries

# Merging dataframes
asymmetries <- rbind(global, asymmetries)
rm(global)

# Tidying
asymmetries %>%
  mutate(Population = factor(Population, levels=unique(Population))) %>%
  gather("Barrier","Asymmetry",2:6) %>%
  mutate(Barrier = factor(Barrier, levels=rev(unique(Barrier)))) %>%
  mutate(Direction = ifelse(Asymmetry > 0,"positive","negative")) -> asymmetries

asymmetries %>%
  na.omit() %>%
  ggplot() +
  facet_wrap(. ~ Population, ncol = 1) +
  geom_col(aes(x=Barrier, y=Asymmetry, fill=Direction), position = position_dodge()) +
  geom_hline(aes(yintercept=0)) +
  coord_flip() +
  scale_fill_manual(values = c('#984ea3','#a65628')) +
  scale_y_continuous(n.breaks = 5, limits = c(-1.2,1.2)) +
  labs(y="Reproductive isolation asymmetry (G♂E♀ - E♂G♀)") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "gray"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        text = element_text(family = "serif", size = s)) -> p

# Converting to dml
p_dml <- rvg::dml(ggobj = p)

# Exporting
officer::read_pptx("../../../00_BasePPTX/PNAS_Tall_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = "../results/01_Asymmetries.pptx")
