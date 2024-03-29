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
  filter(Type == "Heterospecifics") %>%
  mutate(Cross = paste0(Male,"X",Female)) %>% 
  select(Ecology, Cross, Mechanical.Isolation, Mechanical.Tactile.Isolation, Oviposition.Isolation, Fecundity.Isolation, Fertility.Isolation) -> absolutes

# Splitting into reciprocal crosses
asymmetries <- list()

absolutes %>%
  filter(Ecology=="Allopatry") -> asymmetries$allopatry

absolutes %>%
  filter(Ecology=="Sympatry") -> asymmetries$sympatry

rm(absolutes)

# Substracting GE - EG
# Sorting by the selected levels
for(i in names(asymmetries)){
  asymmetries[[i]] %>%
    select(!Ecology) %>%
    t() -> df
  colnames(df) <- df[1,]
  df[-1,] %>%
    as.data.frame() %>% 
    mutate(ElegansXGraellsii = as.numeric(ElegansXGraellsii)) %>%
    mutate(GraellsiiXElegans = as.numeric(GraellsiiXElegans)) %>%
    mutate(GEminusEG = GraellsiiXElegans - ElegansXGraellsii) %>%
    select(GEminusEG) %>%
    mutate(direction = ifelse(GEminusEG>0,"GE","EG"))-> df
  df$Barrier <- row.names(df)
  row.names(df) <- 1:nrow(df)
  asymmetries[[i]] <- df
  print(df)
  rm(df)
}

# Binding dataframes
bind_rows(asymmetries, .id = "Ecology") %>%
  mutate(Barrier = gsub("\\.Isolation","",Barrier)) %>%
  mutate(Barrier = factor(Barrier, levels = rev(unique(Barrier)))) -> asymmetries

# Plotting
asymmetries %>%
  ggplot() +
  facet_wrap(. ~ Ecology, ncol = 1) +
  geom_col(aes(x=Barrier, y=GEminusEG, fill=direction), position = position_dodge()) + 
  geom_hline(aes(yintercept=0)) +
  coord_flip() +
  scale_fill_manual(values = c('#984ea3','#a65628')) +
  scale_y_continuous(breaks = c(-1,-.75,-.5,-.25,0,.25,.5,.75,1), limits = c(-1,1)) +
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
officer::read_pptx("../../../00_BasePPTX/PNAS_Big_Image.pptx") %>%
  # specify object and location of object (full size)
  officer::ph_with(p_dml, ph_location_fullsize()) %>%
  # export slide
  print(target = "../results/01_Asymmetries.pptx")
