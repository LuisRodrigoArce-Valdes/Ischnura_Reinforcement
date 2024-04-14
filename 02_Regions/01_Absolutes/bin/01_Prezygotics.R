# Script to plot the distribution of reproductive barriers using violin plots
rm(list = ls())
library(ggplot2)
library(tidyr)
library(rvg)
library(officer)
library(dplyr)
library(ggh4x)
library(scales)
library(stringr)

# Figure options (plots per barrier) ####
# Text size in points
s <- 10
# line width for Binomial plots
l <- 1
# Select slide size
slide.size <- "../../../00_BasePPTX/PNAS_Large_Image.pptx"

# Tidying sympatrics ####
full.sympatrics <- read.csv("../data/sympatrics_R.csv", header = T, encoding = "UTF-8")
colnames(full.sympatrics)[1] <- "Pair"
summaries.sympatrics <- read.csv("../data/sympatrics_Summaries.csv", header = T, encoding = "UTF-8")
colnames(summaries.sympatrics)[1] <- "Group"

# Tidyversing some filters to our data
full.sympatrics %>%
  filter(Pair != "0") %>%
  select(Group, Mechanical.Sucess, Mechanical.Tactile.Success, ClutchesWEggs, EggsPerClutch, Fertility, Fertility_N) %>% 
  separate(Group, into = c("Type","Cross"), sep = "-") -> full.sympatrics
rm(summaries.sympatrics)

# Tidying allopatrics ####
# Mechanical tidying
allopatrics.mechanicals <- read.csv("../data/Allopatrics_Mechanical_R.csv", header = T, encoding = "UTF-8")
colnames(allopatrics.mechanicals)[1] <- "Pair"
allopatrics.summaries <- read.csv("../data/Allopatrics_Summaries.csv", header = T, encoding = "UTF-8")
colnames(allopatrics.summaries)[1] <- "Group"

# Tidying symmaries
allopatrics.summaries %>%
  filter(Group!="" & Group!=0) %>%
  filter(Include == "Y") -> allopatrics.summaries

allopatrics.mechanicals %>%
  filter(Pair != "0") %>%
  filter(Pair != "") %>%
  filter(Group %in% allopatrics.summaries$Group) %>%
  select(!Pair) %>%
  separate(Group, into = c("Type","Cross"), sep = "-") -> allopatrics.mechanicals

# Fertilities tidying
allopatrics.fertilities <- read.csv("../data/Allopatrics_Fertilities_R.csv", header = T, encoding = "UTF-8")
colnames(allopatrics.fertilities)[1] <- "Pair"

allopatrics.fertilities %>%
  filter(Pair != "0") %>%
  filter(Pair != "") %>%
  filter(Group %in% allopatrics.summaries$Group) %>%
  select(!Pair) %>%
  separate(Group, into = c("Type","Cross"), sep = "-") -> allopatrics.fertilities
rm(allopatrics.summaries)

# Mechanical plot ####
mechanicals <- list()

full.sympatrics %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Mechanical.Sucess) -> mechanicals$Dbsympatrics

allopatrics.mechanicals %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Mechanical.Sucess) -> mechanicals$DbAllopatrics

mechanicals %>%
  bind_rows(.id = "Ecology") %>%
  mutate(Ecology = ifelse(Ecology=="DbAllopatrics","Allopatry","Sympatry")) %>%
  na.omit() %>%
  mutate(Mechanical.Sucess = ifelse(Mechanical.Sucess==1,"Succesfull tandem","Failed tandem")) %>%
  mutate(Mechanical.Sucess = factor(Mechanical.Sucess, levels=c("Succesfull tandem","Failed tandem"))) ->  mechanicals
  
# Plotting
mechanicals %>%
  mutate(Cross = factor(Cross, levels = c("elegansXelegans","graellsiiXgraellsii","elegansXgraellsii","graellsiiXelegans"))) %>% 
  ggplot() +
  facet_wrap(. ~ Cross, scales = "free_x", ncol = 4) +
  geom_bar(aes(x=Ecology, fill=Mechanical.Sucess), position = "fill") +
  scale_fill_manual(values = c('#66c2a5','#fc8d62'), name = "Outcome") +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(y="Mechanical barrier outcome frequency") +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        plot.margin = margin(t=5.5,r=5.5,l=5.5,b=20),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Exporting
read_pptx(slide.size) %>%
  # Adding plot to slide at fullsize
  ph_with(p_dml, ph_location_fullsize()) -> pptx

# Listing tidyed dataframe for statistical tests
tidied <- list()
tidied$mechanicals <- mechanicals

# Mechanical.tactile plot ####
mechanical.tactile <- list()

full.sympatrics %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Mechanical.Tactile.Success) -> mechanical.tactile$Dbsympatrics

allopatrics.mechanicals %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Mechanical.Tactile.Success) -> mechanical.tactile$DbAllopatrics

mechanical.tactile %>%
  bind_rows(.id = "Ecology") %>%
  mutate(Ecology = ifelse(Ecology=="DbAllopatrics","Allopatry","Sympatry")) %>%
  na.omit() %>%
  mutate(Mechanical.Tactile.Success = ifelse(Mechanical.Tactile.Success==1,"Succesfull mating","Failed mating")) %>%
  mutate(Mechanical.Tactile.Success = factor(Mechanical.Tactile.Success, levels=c("Succesfull mating","Failed mating"))) ->  mechanical.tactile

# Plotting
mechanical.tactile %>%
  mutate(Cross = factor(Cross, levels = c("elegansXelegans","graellsiiXgraellsii","elegansXgraellsii","graellsiiXelegans"))) %>% 
  ggplot() +
  facet_wrap(. ~ Cross, scales = "free_x", ncol = 4) +
  geom_bar(aes(x=Ecology, fill=Mechanical.Tactile.Success), position = "fill") +
  scale_fill_manual(values = c('#66c2a5','#fc8d62'), name = "Outcome") +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(y="Mechanical-tactile barrier outcome frequency") +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        plot.margin = margin(t=5.5,r=5.5,l=5.5,b=20),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Exporting
pptx %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(p_dml, ph_location_fullsize()) -> pptx

# Listing tidyed dataframe for statistical tests
tidied$mechanical.tactile <- mechanical.tactile

# Oviposition plot ####
oviposition <- list()

full.sympatrics %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  mutate(ClutchesWEggs=ifelse(ClutchesWEggs>0,1,ClutchesWEggs)) %>%
  select(Cross, ClutchesWEggs) -> oviposition$Dbsympatrics

allopatrics.fertilities %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  mutate(ClutchesWEggs=ifelse(ClutchesWEggs>0,1,ClutchesWEggs)) %>%
  select(Cross, ClutchesWEggs) -> oviposition$DbAllopatrics

oviposition %>%
  bind_rows(.id = "Ecology") %>%
  mutate(Ecology = ifelse(Ecology=="DbAllopatrics","Allopatry","Sympatry")) %>%
  na.omit() %>%
  mutate(ClutchesWEggs = ifelse(ClutchesWEggs==1,"Succesfull oviposition","Failed oviposition")) %>%
  mutate(ClutchesWEggs = factor(ClutchesWEggs, levels=c("Succesfull oviposition","Failed oviposition"))) -> oviposition

# Listing tidyed dataframe for statistical tests
tidied$oviposition <- oviposition

# Plotting
oviposition %>%
  mutate(Cross = factor(Cross, levels = c("elegansXelegans","graellsiiXgraellsii","elegansXgraellsii","graellsiiXelegans"))) %>%
  ggplot() +
  facet_wrap(. ~ Cross, scales = "free_x", ncol = 4) +
  geom_bar(aes(x=Ecology, fill=ClutchesWEggs), position = "fill") +
  scale_fill_manual(values = c('#66c2a5','#fc8d62'), name = "Outcome") +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(y="Oviposition barrier outcome frequency") +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        plot.margin = margin(t=5.5,r=5.5,l=5.5,b=20),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Adding plot to new slide at fullsize
pptx %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(p_dml, ph_location_fullsize()) -> pptx

# Fecundity plot ####
fecundity <- list()

full.sympatrics %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, EggsPerClutch) -> fecundity$Dbsympatrics

allopatrics.fertilities %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, EggsPerClutch) -> fecundity$DbAllopatrics

fecundity %>%
  bind_rows(.id = "Ecology") %>%
  mutate(Ecology = ifelse(Ecology=="DbAllopatrics","Allopatry","Sympatry")) %>%
  na.omit() -> fecundity

# Listing tidyed dataframe for statistical tests
tidied$fecundity <- fecundity

# Estimating absolute and relative frequencies of barrier outputs
fecundity %>%
  mutate(Cross = factor(Cross, levels = c("elegansXelegans","graellsiiXgraellsii","elegansXgraellsii","graellsiiXelegans"))) %>%
  ggplot() +
  facet_wrap(. ~ Cross, scales = "free_x", ncol = 4) +
  geom_violin(aes(x=Ecology, y=EggsPerClutch, color = Ecology), alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
  geom_point(aes(x=Ecology, y=EggsPerClutch), size = 1) +
  scale_color_manual(values = c("#7570b3","#e7298a")) +
  theme_classic() +
  labs(y="Fecundity (Average eggs per clutch per female)") +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        plot.margin = margin(t=5.5,r=5.5,l=5.5,b=20),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Adding plot to new slide at fullsize
pptx %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(p_dml, ph_location_fullsize()) -> pptx

# NEW PLOT LIMITING Y AXIS
fecundity %>%
  mutate(Cross = factor(Cross, levels = c("elegansXelegans","graellsiiXgraellsii","elegansXgraellsii","graellsiiXelegans"))) %>%
  ggplot() +
  facet_wrap(. ~ Cross, scales = "free_x", ncol = 4) +
  geom_violin(aes(x=Ecology, y=EggsPerClutch, color = Ecology), alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
  geom_point(aes(x=Ecology, y=EggsPerClutch), size = 1) +
  scale_color_manual(values = c("#7570b3","#e7298a")) +
  scale_y_continuous(limits = c(0,400)) +
  theme_classic() +
  labs(y="Fecundity (Average eggs per clutch per female)") +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        plot.margin = margin(t=5.5,r=5.5,l=5.5,b=20),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Adding plot to new slide at fullsize
pptx %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(p_dml, ph_location_fullsize()) -> pptx

# Fertility plot ####
fertility <- list()

full.sympatrics %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Fertility, Fertility_N) -> fertility$Dbsympatrics

allopatrics.fertilities %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Fertility, Fertility_N) -> fertility$DbAllopatrics

fertility %>%
  bind_rows(.id = "Ecology") %>%
  mutate(Ecology = ifelse(Ecology=="DbAllopatrics","Allopatry","Sympatry")) %>%
  na.omit() -> fertility

# Listing tidyed dataframe for statistical tests
tidied$fertility <- fertility

# Plotting
fertility %>%
  mutate(Cross = factor(Cross, levels = c("elegansXelegans","graellsiiXgraellsii","elegansXgraellsii","graellsiiXelegans"))) %>%
  ggplot() +
  facet_wrap(. ~ Cross, scales = "free_x", ncol = 4) +
  geom_violin(aes(x=Ecology, y=Fertility, color= Ecology), alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
  geom_point(aes(x=Ecology, y=Fertility), size = 1) +
  scale_color_manual(values = c("#7570b3","#e7298a")) +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(y="Fertility (Ratio of fertile eggs per female)") +
  theme(axis.title.x = element_blank(),
        text = element_text(family = "serif", size = s),
        plot.margin = margin(t=5.5,r=5.5,l=5.5,b=20),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Adding plot to new slide at fullsize
pptx %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(p_dml, ph_location_fullsize()) -> pptx

print(pptx, target = "../figures/01_Prezygotics.pptx")

# Saving tidied data for statistics ####
save(tidied, file = "../results/01_Prezygotics.Rdata")
