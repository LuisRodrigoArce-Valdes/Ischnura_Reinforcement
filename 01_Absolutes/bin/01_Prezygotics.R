# Script to plot the distribution of reproductive barriers using violin plots
rm(list = ls())
library(ggplot2)
library(tidyr)
library(rvg)
library(officer)
library(dplyr)
library(ggh4x)
library(scales)

# Figure options (plots per barrier) ####
# Text size in points
s <- 10
# X axis text size in points
tx <- 6
# x axis angle
a <- 10
# X axis text size in points for multiple plot
tx5 <- 8
# x axis angle for multiple plots
a5 <- 20
# line width for Binomial plots
l <- 1
# Select slide size
slide.size <- "../../00_BasePPTX/PNAS_Large_Image.pptx"

# Tidying 2019 ####
full.2019 <- read.csv("../data/2019_R.csv", header = T, encoding = "UTF-8")
colnames(full.2019)[1] <- "Pair"
summaries.2019 <- read.csv("../data/2019_Summaries.csv", header = T, encoding = "UTF-8")
colnames(summaries.2019)[1] <- "Group"

# Tidyversing some filters to our data
full.2019 %>%
  filter(Group %in% summaries.2019[summaries.2019$Include == "Y","Group"]) %>%
  filter(Pair != "0") %>%
  select(Group, Mechanical.Sucess, Mechanical.Tactile.Success, ClutchesWEggs, EggsPerClutch, Fertility, Fertility_N) %>%
  separate(Group, into = c("Type","Cross","Population"), sep = "-") -> full.2019
rm(summaries.2019)

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
  separate(Group, into = c("Type","Cross","Population"), sep = "-") -> allopatrics.mechanicals

# Fertilities tidying
allopatrics.fertilities <- read.csv("../data/Allopatrics_Fertilities_R.csv", header = T, encoding = "UTF-8")
colnames(allopatrics.fertilities)[1] <- "Pair"

allopatrics.fertilities %>%
  filter(Pair != "0") %>%
  filter(Pair != "") %>%
  filter(Group %in% allopatrics.summaries$Group) %>%
  select(!Pair) %>%
  separate(Group, into = c("Type","Cross","Population"), sep = "-") -> allopatrics.fertilities
rm(allopatrics.summaries)

# Tidying 2001 ####
mechanicals.2001 <- read.csv("../data/2001_Mechanical_R.csv", header = T, encoding = "UTF-8")
colnames(mechanicals.2001)[1] <- "Pair"
summaries.2001 <- read.csv("../data/2001_Summaries.csv", header = T, encoding = "UTF-8")
colnames(summaries.2001)[1] <- "Group"

# Tidying symmaries
summaries.2001 %>%
  filter(Group!="" & Group!=0) %>%
  filter(Include == "Y") -> summaries.2001

mechanicals.2001 %>%
  filter(Pair != "0") %>%
  filter(Pair != "") %>%
  filter(Group %in% summaries.2001$Group) %>%
  select(!Pair) %>%
  separate(Group, into = c("Type","Cross","Population"), sep = "-") -> mechanicals.2001

# Fertilities tidying
fertilities.2001 <- read.csv("../data/2001_Fertilities_R.csv", header = T, encoding = "UTF-8")
colnames(fertilities.2001)[1] <- "Pair"

fertilities.2001 %>%
  filter(Pair != "0") %>%
  filter(Pair != "") %>%
  filter(Group %in% summaries.2001$Group) %>%
  select(!Pair) %>%
  separate(Group, into = c("Type","Cross","Population"), sep = "-") -> fertilities.2001
rm(summaries.2001)

# Mechanical plot ####
mechanicals <- list()

full.2019 %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, Mechanical.Sucess) -> mechanicals$Db2019

allopatrics.mechanicals %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, Mechanical.Sucess) -> mechanicals$DbAllopatrics

mechanicals.2001 %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, Mechanical.Sucess) -> mechanicals$Db2001

mechanicals %>%
  bind_rows(.id = "Ecology") %>%
  mutate(Ecology = ifelse(Ecology=="DbAllopatrics","Allopatry","Sympatry")) %>%
  na.omit() %>%
  mutate(Mechanical.Sucess = ifelse(Mechanical.Sucess==1,"Succesfull tandem","Failed tandem")) %>%
  mutate(Mechanical.Sucess = factor(Mechanical.Sucess, levels=c("Succesfull tandem","Failed tandem"))) -> mechanicals
  
# Estimating sample size per population
as.data.frame(table(mechanicals$Population)) -> sample.size

# Merging sample sizes
mechanicals %>%
  left_join(sample.size, by=c("Population"="Var1")) %>%
  mutate(Population = paste0(Population," (",Freq,")")) %>%
  select(!Freq) %>%
  mutate(Cross = factor(Cross, levels=c("elegansXelegans","graellsiiXgraellsii",
                                        "elegansXgraellsii","graellsiiXelegans"))) -> mechanicals
rm(sample.size)

# Estimating absolute and relative frequencies of barrier outputs
mechanicals %>%
  ggplot() +
  facet_grid2(cols = vars(Cross), rows = vars(Ecology), scales = "free_x", independent = "x") +
  geom_bar(aes(x=Population, fill=Mechanical.Sucess), position = "fill") +
  scale_fill_manual(values = c('#66c2a5','#fc8d62'), name = "Outcome") +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(y="Mechanical barrier outcome frequency") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = tx, angle = a, hjust = 1),
        text = element_text(family = "serif", size = s),
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

# Removing populations for combined plot
mechanicals %>%
  select(!Population) -> mechanicals

# Estimating sample sizes
mechanicals %>%
  mutate(EcologyCross = paste0(Ecology,"-",Cross)) -> mechanicals

# Counting
as.data.frame(table(mechanicals$EcologyCross)) -> sample.size

# Merging sample sizes
mechanicals %>%
  left_join(sample.size, by=c("EcologyCross"="Var1")) %>%
  mutate(Ecology2 = paste0(Ecology," (",Freq,")")) %>%
  select(!Freq & !EcologyCross) %>%
  mutate(Cross = factor(Cross, levels=c("elegansXelegans","graellsiiXgraellsii",
                                        "elegansXgraellsii","graellsiiXelegans"))) -> mechanicals
rm(sample.size)

# Plotting
mechanicals %>%
  ggplot() +
  facet_grid2(cols = vars(Cross), scales = "free_x", independent = "x") +
  geom_bar(aes(x=Ecology2, fill=Mechanical.Sucess,linetype = Ecology), linewidth = l, color="black", position = "fill") +
  scale_linetype_manual(values = c("solid","blank")) +
  scale_fill_manual(values = c('#66c2a5','#fc8d62'), name = "Outcome") +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(title="A", y="Mechanical barrier outcome frequency") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = tx5, angle = a5, hjust = 1),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p.mechanicals

# Mechanical-tactile plot ####
mechanical.tactile <- list()

full.2019 %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, Mechanical.Tactile.Success) -> mechanical.tactile$Db2019

allopatrics.mechanicals %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, Mechanical.Tactile.Success) -> mechanical.tactile$DbAllopatrics

mechanicals.2001 %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, Mechanical.Tactile.Success) -> mechanical.tactile$Db2001

mechanical.tactile %>%
  bind_rows(.id = "Ecology") %>%
  mutate(Ecology = ifelse(Ecology=="DbAllopatrics","Allopatry","Sympatry")) %>%
  na.omit() %>%
  mutate(Mechanical.Tactile.Success = ifelse(Mechanical.Tactile.Success==1,"Succesfull mating","Failed mating")) %>%
  mutate(Mechanical.Tactile.Success = factor(Mechanical.Tactile.Success, levels=c("Succesfull mating","Failed mating"))) -> mechanical.tactile

# Estimating sample size per population
as.data.frame(table(mechanical.tactile$Population)) -> sample.size

# Merging sample sizes
mechanical.tactile %>%
  left_join(sample.size, by=c("Population"="Var1")) %>%
  mutate(Population = paste0(Population," (",Freq,")")) %>%
  select(!Freq) %>%
  mutate(Cross = factor(Cross, levels=c("elegansXelegans","graellsiiXgraellsii",
                                        "elegansXgraellsii","graellsiiXelegans"))) -> mechanical.tactile
rm(sample.size)

# Listing tidyed dataframe for statistical tests
tidied$mechanical.tactile <- mechanical.tactile

# Estimating absolute and relative frequencies of barrier outputs
mechanical.tactile %>%
  ggplot() +
  facet_grid2(cols = vars(Cross), rows = vars(Ecology), scales = "free_x", independent = "x") +
  geom_bar(aes(x=Population, fill=Mechanical.Tactile.Success), position = "fill") +
  scale_fill_manual(values = c('#66c2a5','#fc8d62'), name = "Outcome") +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(y="Mechanical-Tactile barrier outcome frequency") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = tx, angle = a, hjust = 1),
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

# Estimating sample sizes
mechanical.tactile %>%
  mutate(EcologyCross = paste0(Ecology,"-",Cross)) -> mechanical.tactile

# Counting
as.data.frame(table(mechanical.tactile$EcologyCross)) -> sample.size

# Merging sample sizes
mechanical.tactile %>%
  left_join(sample.size, by=c("EcologyCross"="Var1")) %>%
  mutate(Ecology2 = paste0(Ecology," (",Freq,")")) %>%
  select(!Freq & !EcologyCross) %>%
  mutate(Cross = factor(Cross, levels=c("elegansXelegans","graellsiiXgraellsii",
                                        "elegansXgraellsii","graellsiiXelegans"))) -> mechanical.tactile
rm(sample.size)

# Plotting
mechanical.tactile %>%
  ggplot() +
  facet_grid2(cols = vars(Cross), scales = "free_x", independent = "x") +
  geom_bar(aes(x=Ecology2, fill=Mechanical.Tactile.Success,linetype = Ecology), linewidth = l, color="black", position = "fill") +
  scale_linetype_manual(values = c("solid","blank")) +
  scale_fill_manual(values = c('#66c2a5','#fc8d62'), name = "Outcome") +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(title="B", y="Mechanical-Tactile barrier outcome frequency") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = tx5, angle = a5, hjust = 1),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p.mechanical.tactile

# Oviposition plot ####
oviposition <- list()

full.2019 %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  mutate(ClutchesWEggs=ifelse(ClutchesWEggs>0,1,ClutchesWEggs)) %>%
  select(Cross, Population, ClutchesWEggs) -> oviposition$Db2019

allopatrics.fertilities %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  mutate(ClutchesWEggs=ifelse(ClutchesWEggs>0,1,ClutchesWEggs)) %>%
  select(Cross, Population, ClutchesWEggs) -> oviposition$DbAllopatrics

fertilities.2001 %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  mutate(ClutchesWEggs=ifelse(ClutchesWEggs>0,1,ClutchesWEggs)) %>%
  select(Cross, Population, ClutchesWEggs) -> oviposition$Db2001

oviposition %>%
  bind_rows(.id = "Ecology") %>%
  mutate(Ecology = ifelse(Ecology=="DbAllopatrics","Allopatry","Sympatry")) %>%
  na.omit() %>%
  mutate(ClutchesWEggs = ifelse(ClutchesWEggs==1,"Succesfull oviposition","Failed oviposition")) %>%
  mutate(ClutchesWEggs = factor(ClutchesWEggs, levels=c("Succesfull oviposition","Failed oviposition"))) -> oviposition

# Estimating sample size per population
as.data.frame(table(oviposition$Population)) -> sample.size

# Merging sample sizes
oviposition %>%
  left_join(sample.size, by=c("Population"="Var1")) %>%
  mutate(Population = paste0(Population," (",Freq,")")) %>%
  select(!Freq) %>%
  mutate(Cross = factor(Cross, levels=c("elegansXelegans","graellsiiXgraellsii",
                                        "elegansXgraellsii","graellsiiXelegans"))) -> oviposition
rm(sample.size)

# Listing tidyed dataframe for statistical tests
tidied$oviposition <- oviposition

# Estimating absolute and relative frequencies of barrier outputs
oviposition %>%
  ggplot() +
  facet_grid2(cols = vars(Cross), rows = vars(Ecology), scales = "free_x", independent = "x") +
  geom_bar(aes(x=Population, fill=ClutchesWEggs), position = "fill") +
  scale_fill_manual(values = c('#66c2a5','#fc8d62'), name = "Outcome") +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(y="Oviposition barrier outcome frequency") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = tx, angle = a, hjust = 1),
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

# Estimating sample sizes
oviposition %>%
  mutate(EcologyCross = paste0(Ecology,"-",Cross)) -> oviposition

# Counting
as.data.frame(table(oviposition$EcologyCross)) -> sample.size

# Merging sample sizes
oviposition %>%
  left_join(sample.size, by=c("EcologyCross"="Var1")) %>%
  mutate(Ecology2 = paste0(Ecology," (",Freq,")")) %>%
  select(!Freq & !EcologyCross) %>%
  mutate(Cross = factor(Cross, levels=c("elegansXelegans","graellsiiXgraellsii",
                                        "elegansXgraellsii","graellsiiXelegans"))) -> oviposition
rm(sample.size)

# Plotting
oviposition %>%
  ggplot() +
  facet_grid2(cols = vars(Cross), scales = "free_x", independent = "x") +
  geom_bar(aes(x=Ecology, fill=ClutchesWEggs,linetype = Ecology), linewidth = l, color="black", position = "fill") +
  scale_linetype_manual(values = c("solid","blank")) +
  scale_fill_manual(values = c('#66c2a5','#fc8d62'), name = "Outcome") +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(title="C", y="Oviposition barrier outcome frequency") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = tx5, angle = a5, hjust = 1),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p.oviposition

# Fecundity plot ####
fecundity <- list()

full.2019 %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, EggsPerClutch) -> fecundity$Db2019

allopatrics.fertilities %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, EggsPerClutch) -> fecundity$DbAllopatrics

fertilities.2001 %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, EggsPerClutch) -> fecundity$Db2001

fecundity %>%
  bind_rows(.id = "Ecology") %>%
  mutate(Ecology = ifelse(Ecology=="DbAllopatrics","Allopatry","Sympatry")) %>%
  na.omit() -> fecundity

# Estimating sample size per population
as.data.frame(table(fecundity$Population)) -> sample.size

# Merging sample sizes
fecundity %>%
  left_join(sample.size, by=c("Population"="Var1")) %>%
  mutate(Population = paste0(Population," (",Freq,")")) %>%
  select(!Freq) %>%
  mutate(Cross = factor(Cross, levels=c("elegansXelegans","graellsiiXgraellsii",
                                        "elegansXgraellsii","graellsiiXelegans"))) -> fecundity
rm(sample.size)

# Listing tidyed dataframe for statistical tests
tidied$fecundity <- fecundity

# Estimating absolute and relative frequencies of barrier outputs
fecundity %>%
  ggplot() +
  facet_grid2(cols = vars(Cross), rows = vars(Ecology), scales = "free_x", independent = "x") +
  geom_violin(aes(x=Population, y=EggsPerClutch), alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
  geom_point(aes(x=Population, y=EggsPerClutch), size = 1) +
  theme_classic() +
  labs(y="Fecundity (Eggs per clutch)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = tx, angle = a, hjust = 1),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Adding plot to new slide at fullsize
pptx %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(p_dml, ph_location_fullsize()) -> pptx

print(pptx, target = "../figures/01_PopsIsolation.pptx")

# Removing populations for future plot
fecundity %>%
  select(!Population) -> fecundity

# Estimating sample sizes
fecundity %>%
  mutate(EcologyCross = paste0(Ecology,"-",Cross)) -> fecundity

# Counting
as.data.frame(table(fecundity$EcologyCross)) -> sample.size

# Merging sample sizes
fecundity %>%
  left_join(sample.size, by=c("EcologyCross"="Var1")) %>%
  mutate(Ecology2 = paste0(Ecology," (",Freq,")")) %>%
  select(!Freq & !EcologyCross) %>%
  mutate(Cross = factor(Cross, levels=c("elegansXelegans","graellsiiXgraellsii",
                                        "elegansXgraellsii","graellsiiXelegans"))) -> fecundity
rm(sample.size)

# Plotting
fecundity %>%
  ggplot() +
  facet_grid2(cols = vars(Cross), scales = "free_x", independent = "x") +
  geom_violin(aes(x=Ecology2, y=EggsPerClutch, fill=Ecology), alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
  geom_point(aes(x=Ecology2, y=EggsPerClutch, shape=Ecology), size = 1) +
  scale_shape_manual(values = c(17, 16)) +
  scale_fill_manual(values = c("#7570b3","#e7298a")) +  theme_classic() +
  labs(title="D", y="Fecundity (Eggs per clutch)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = tx5, angle = a5, hjust = 1),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p.fecundity

# Fertility plot ####
fertility <- list()

full.2019 %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, Fertility, Fertility_N) -> fertility$Db2019

allopatrics.fertilities %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, Fertility, Fertility_N) -> fertility$DbAllopatrics

fertilities.2001 %>%
  filter(Type=="Conspecifics" | Type=="Heterospecifics") %>%
  select(Cross, Population, Fertility, Fertility_N) -> fertility$Db2001

fertility %>%
  bind_rows(.id = "Ecology") %>%
  mutate(Ecology = ifelse(Ecology=="DbAllopatrics","Allopatry","Sympatry")) %>%
  na.omit() -> fertility

# Estimating sample size per population
as.data.frame(table(fertility$Population)) -> sample.size

# Merging sample sizes
fertility %>%
  left_join(sample.size, by=c("Population"="Var1")) %>%
  mutate(Population = paste0(Population," (",Freq,")")) %>%
  select(!Freq) %>%
  mutate(Cross = factor(Cross, levels=c("elegansXelegans","graellsiiXgraellsii",
                                        "elegansXgraellsii","graellsiiXelegans"))) -> fertility
rm(sample.size)

# Listing tidyed dataframe for statistical tests
tidied$fertility <- fertility

# Estimating absolute and relative frequencies of barrier outputs
fertility %>%
  ggplot() +
  facet_grid2(cols = vars(Cross), rows = vars(Ecology), scales = "free_x", independent = "x") +
  geom_violin(aes(x=Population, y=Fertility), alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
  geom_point(aes(x=Population, y=Fertility), size = 1) +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(y="Fertility (Ratio of fertile eggs)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = tx, angle = a, hjust = 1),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p

# Converting to dml
p_dml <- dml(ggobj = p)

# Adding plot to new slide at fullsize
pptx %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(p_dml, ph_location_fullsize()) -> pptx

print(pptx, target = "../figures/01_PopsIsolation.pptx")

# Removing populations for future plot
fertility %>%
  select(!Population) -> fertility

# Estimating sample sizes
fertility %>%
  mutate(EcologyCross = paste0(Ecology,"-",Cross)) -> fertility

# Counting
as.data.frame(table(fertility$EcologyCross)) -> sample.size

# Merging sample sizes
fertility %>%
  left_join(sample.size, by=c("EcologyCross"="Var1")) %>%
  mutate(Ecology2 = paste0(Ecology," (",Freq,")")) %>%
  select(!Freq & !EcologyCross) %>%
  mutate(Cross = factor(Cross, levels=c("elegansXelegans","graellsiiXgraellsii",
                                        "elegansXgraellsii","graellsiiXelegans"))) -> fertility
rm(sample.size)

# Plotting
fertility %>%
  ggplot() +
  facet_grid2(cols = vars(Cross), scales = "free_x", independent = "x") +
  geom_violin(aes(x=Ecology2, y=Fertility, fill=Ecology), alpha = 0.5, draw_quantiles = 0.5, scale = "width") +
  geom_point(aes(x=Ecology2, y=Fertility, shape=Ecology), size = 1) +
  scale_shape_manual(values = c(17, 16)) +
  scale_fill_manual(values = c("#7570b3","#e7298a")) +
  scale_y_continuous(labels = percent) +
  theme_classic() +
  labs(title="E", y="Fertility (Ratio of fertile eggs)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = tx5, angle = a5, hjust = 1),
        text = element_text(family = "serif", size = s),
        legend.position="none") -> p.fertility

# All barriers plot ####
# Select slide size
slide.size <- "../../00_BasePPTX/Multiple_5.pptx"

# Creating powerpoint slides
read_pptx(slide.size) %>%
  ph_with(dml(ggobj = p.mechanicals), ph_location_fullsize()) %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(dml(ggobj = p.mechanical.tactile), ph_location_fullsize()) %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(dml(ggobj = p.oviposition), ph_location_fullsize()) %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(dml(ggobj = p.fecundity), ph_location_fullsize()) %>%
  add_slide(layout = "En blanco", master = "Tema de Office") %>%
  ph_with(dml(ggobj = p.fertility), ph_location_fullsize()) %>%
  print(target = "../figures/02_MergedIsolation.pptx")

# Saving tidied data for statistics ####
save(tidied, file = "../results/01_Prezygotics.Rdata")
