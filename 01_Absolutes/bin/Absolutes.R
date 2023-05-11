# Script to plot the distribution of reproductive barriers using violin plots
rm(list = ls())
library(ggplot2)
library(tidyr)
library(rvg)
library(officer)
library(dplyr)

# Figure options ####
# Text size in points
s <- 12
# Select slide size
slide.size <- "../../00_BasePPTX/PNAS_Tall_Image.pptx"
# To estimate isolation use "iso" (1-success) if you want geneflow use "flow" (success)
i <- "flow"

# Tidying 2019 ####
full.2019 <- read.csv("../data/2019_R.csv", header = T, encoding = "UTF-8")
colnames(full.2019)[1] <- "Pair"

full.2019 %>%
  # removing extra row
  filter(Pair != "0") %>%
  select(Group, Mechanical.Sucess, Mechanical.Tactile.Success, ClutchesWEggs, EggsPerClutch, Fertility) %>%
  separate(Group, into = c("Generation","Cross","Population"), sep = "-") -> full.2019

# Tidying allopatrics ####
# Mechanical tidying
allopatrics.mechanicals <- read.csv("../data/Allopatrics_Mechanical_R.csv", header = T, encoding = "UTF-8")
colnames(allopatrics.mechanicals)[1] <- "Pair"

allopatrics.mechanicals %>%
  filter(Pair != "0") %>%
  filter(Pair != "") %>%
  select(!Pair) %>%
  separate(Group, into = c("Generation","Cross","Population"), sep = "-") -> allopatrics.mechanicals

# Fertilities tidying
allopatrics.fertilities <- read.csv("../data/Allopatrics_Fertilities_R.csv", header = T, encoding = "UTF-8")
colnames(allopatrics.fertilities)[1] <- "Pair"

allopatrics.fertilities %>%
  filter(Pair != "0") %>%
  filter(Pair != "") %>%
  select(!Pair) %>%
  separate(Group, into = c("Generation","Cross","Population"), sep = "-") -> allopatrics.fertilities

# Tidying 2000 ####

# Mechanical plot ####
mechanicals <- list()

full.2019 %>%
  filter(Generation=="F0") %>%
  select(Cross, Population, Mechanical.Sucess) -> mechanicals$Db2019

allopatrics.mechanicals %>%
  filter(Generation=="F0") %>%
  select(Cross, Population, Mechanical.Sucess) -> mechanicals$DbAllopatrics

mechanicals %>%
  bind_rows(.id = "Database") -> mechanicals

if(i=="iso"){
  mechanicals$Mechanical.Sucess = 1 - mechanicals$Mechanical.Sucess
}

mechanicals %>%
  ggplot() +
  facet_wrap(Cross ~ Database, ncol = 2, scales = "free_x", drop = F) +
  geom_violin(aes(x=Population, y=Mechanical.Sucess, fill=Database), alpha=0.50) +
  geom_jitter(aes(x=Population, y=Mechanical.Sucess), height = 0) +
  scale_fill_manual(values = c('navyblue','red3')) + #'#66c2a5'
  theme_classic() +
  labs(y=ifelse(i=="iso", paste0("Mechanical Reproductive ", "Isolation"), paste0("Mechanical Reproductive ", "Success"))) +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Exporting
read_pptx(slide.size) %>%
  # Adding plot to slide at fullsize
  ph_with(p_dml, ph_location_fullsize()) -> pptx

# Removing populations for future plot
mechanicals %>%
  select(!Population) -> mechanicals


# Mechanical-tactile plot ####
mechanical.tactile <- list()

full.2019 %>%
  filter(Generation=="F0") %>%
  select(Cross, Population, Mechanical.Tactile.Success) -> mechanical.tactile$Db2019

allopatrics.mechanicals %>%
  filter(Generation=="F0") %>%
  select(Cross, Population, Mechanical.Tactile.Success) -> mechanical.tactile$DbAllopatrics

mechanical.tactile %>%
  bind_rows(.id = "Database") -> mechanical.tactile

if(i=="iso"){
  mechanical.tactile$Mechanical.Tactile.Success = 1 - mechanical.tactile$Mechanical.Tactile.Success
}

mechanical.tactile[complete.cases(mechanical.tactile),] %>%
  ggplot() +
  facet_wrap(Cross ~ Database, ncol = 2, scales = "free_x", drop = F) +
  geom_violin(aes(x=Population, y=Mechanical.Tactile.Success, fill=Database), alpha=0.50) +
  geom_jitter(aes(x=Population, y=Mechanical.Tactile.Success), height = 0) +
  scale_fill_manual(values = c('navyblue','red3')) + #'#66c2a5'
  theme_classic() +
  labs(y=ifelse(i=="iso", paste0("Mechanical-Tactile Reproductive ", "Isolation"), paste0("Mechanical-Tactile Reproductive ", "Success"))) +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

  # Adding plot to new slide at fullsize
pptx %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(p_dml, ph_location_fullsize()) -> pptx

# Removing populations for future plot
mechanical.tactile %>%
  select(!Population) -> mechanical.tactile

# Oviposition plot ####
oviposition <- list()

full.2019 %>%
  filter(Generation=="F0") %>%
  mutate(ClutchesWEggs=ifelse(ClutchesWEggs>0,1,ClutchesWEggs)) %>%
  select(Cross, Population, ClutchesWEggs) -> oviposition$Db2019

allopatrics.fertilities %>%
  filter(Generation=="F0") %>%
  mutate(ClutchesWEggs=ifelse(ClutchesWEggs>0,1,ClutchesWEggs)) %>%
  select(Cross, Population, ClutchesWEggs) -> oviposition$DbAllopatrics

oviposition %>%
  bind_rows(.id = "Database") -> oviposition

if(i=="iso"){
  oviposition$ClutchesWEggs = 1 - oviposition$ClutchesWEggs
}

oviposition[complete.cases(oviposition),] %>%
  ggplot() +
  facet_wrap(Cross ~ Database, ncol = 2, scales = "free_x", drop = F) +
  geom_violin(aes(x=Population, y=ClutchesWEggs, fill=Database), alpha=0.50) +
  geom_jitter(aes(x=Population, y=ClutchesWEggs), height = 0) +
  scale_fill_manual(values = c('navyblue','red3')) + #'#66c2a5'
  theme_classic() +
  labs(y=ifelse(i=="iso", paste0("Ovipostion Reproductive ", "Isolation"), paste0("Oviposition Reproductive ", "Success"))) +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Adding plot to new slide at fullsize
pptx %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(p_dml, ph_location_fullsize()) -> pptx

# Removing populations for future plot
oviposition %>%
  select(!Population) -> oviposition

# Fecundity plot ####
fecundity <- list()

full.2019 %>%
  filter(Generation=="F0") %>%
  select(Cross, Population, EggsPerClutch) -> fecundity$Db2019

allopatrics.fertilities %>%
  filter(Generation=="F0") %>%
  select(Cross, Population, EggsPerClutch) -> fecundity$DbAllopatrics

fecundity %>%
  bind_rows(.id = "Database") -> fecundity

fecundity[complete.cases(fecundity),] %>%
  ggplot() +
  facet_wrap(Cross ~ Database, ncol = 2, scales = "free_x", drop = F) +
  geom_violin(aes(x=Population, y=EggsPerClutch, fill=Database), alpha=0.50) +
  geom_jitter(aes(x=Population, y=EggsPerClutch), height = 0) +
  scale_fill_manual(values = c('navyblue','red3')) + #'#66c2a5'
  theme_classic() +
  labs(y="Fecundity (Eggs Per Clutch)") +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Adding plot to new slide at fullsize
pptx %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(p_dml, ph_location_fullsize()) -> pptx

# Removing populations for future plot
fecundity %>%
  select(!Population) -> fecundity

print(pptx, target = "../figures/01_PopsIsolation.pptx")

# Fertility plot ####
fertility <- list()

full.2019 %>%
  filter(Generation=="F0") %>%
  select(Cross, Population, Fertility) -> fertility$Db2019

allopatrics.fertilities %>%
  filter(Generation=="F0") %>%
  select(Cross, Population, Fertility) -> fertility$DbAllopatrics

fertility %>%
  bind_rows(.id = "Database")  -> fertility

if(i=="iso"){
  fertility$Fertility = 1 - fertility$Fertility
}

fertility[complete.cases(fertility),] %>%
  ggplot() +
  facet_wrap(Cross ~ Database, ncol = 2, scales = "free_x", drop = F) +
  geom_violin(aes(x=Population, y=Fertility, fill=Database), alpha=0.50) +
  geom_jitter(aes(x=Population, y=Fertility), height = 0) +
  scale_fill_manual(values = c('navyblue','red3')) + #'#66c2a5'
  theme_classic() +
  labs(y=ifelse(i=="iso", paste0("Fertility Reproductive ", "Isolation"), paste0("Fertility Reproductive ", "Success"))) +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Adding plot to new slide at fullsize
pptx %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(p_dml, ph_location_fullsize()) -> pptx

# Removing populations for future plot
fertility %>%
  select(!Population) -> fertility

print(pptx, target = "../figures/01_PopsIsolation.pptx")

# All barriers plot ####
barriers <- list(mechanicals = mechanicals,
                 mechanical.tactile = mechanical.tactile, 
                 oviposition = oviposition,
                 fecundity = fecundity,
                 fertility = fertility)
rm(mechanicals, mechanical.tactile, oviposition, fecundity, fertility)

for(i in names(barriers)){
  colnames(barriers[[i]])[3] <- ifelse(i=="iso","Isolation","Success")
}

barriers %>%
  bind_rows(barriers, .id = "Barrier") -> barriers
