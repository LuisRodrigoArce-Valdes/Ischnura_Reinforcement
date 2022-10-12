rm(list = ls())

# Calling up libraries
library(tidyr)
library(ggplot2)
library(ggtext)

# With this script we will make absolute isolation barriers bar plots
barriers <- read.csv("../data/Absolutes.csv", header = T, encoding = "UTF-8")

# Renaming Tactile Barrier
colnames(barriers)[8] <- "Mechanical-Tactile"

# Prezygotic barriers
prezygotics <- barriers[barriers$Generation=="F0",-c(4,5,6)]

# Heterospecifics crosses plots:
heterospecifics <- barriers[barriers$Type=="Heterospecific",-c(4,5,6)]

# Postzygotic barriers
heterospecifics <- heterospecifics[heterospecifics$Generation!="F0",]

# Pasting generation and type of cross
heterospecifics$Type <- paste0(heterospecifics$Generation, "-", heterospecifics$Cross)

# Removing more columns
heterospecifics <- heterospecifics[,-c(2,3)]

# Factorying type
heterospecifics$Type <- factor(heterospecifics$Type, levels = c("F1-E♂H♀","F1-H♂E♀","F2-E♂H♀","F2-H♂E♀",
                                                                "F1-G♂H♀","F1-H♂G♀","F2-G♂H♀","F2-H♂G♀",
                                                                "F1-H♂H♀","F2-H♂H♀"))

# Tidying
heterospecifics <- gather(heterospecifics, "Barrier", "Isolation", 2:6)

# Factoring Barriers
heterospecifics$Barrier <- factor(heterospecifics$Barrier, levels = unique(heterospecifics$Barrier))

# Replacing 0 by a tiny value (to increase a little bit the bar)
heterospecifics[heterospecifics==0] <- 0.02

# Ordering by Type
heterospecifics <- heterospecifics[order(heterospecifics$Type),]

# Plotting
plotA <- ggplot(heterospecifics) +
  geom_col(aes(x=Barrier, y=Isolation, fill=Year, color=Year),size=0.15, position = position_dodge()) +
  geom_hline(aes(yintercept=0))+
  scale_fill_manual(values = c('#66c2a5','#8da0cb','#fc8d62')) +
  scale_color_manual(values = c('#66c2a5','#8da0cb','#fc8d62')) +
  facet_wrap(. ~ Type, nrow = 4, dir = "v", scales = "free_y") +
  scale_y_continuous(limits = c(NA, 1), n.breaks = 7) +
  labs(y="Absolute Reproductive Isolation") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "gray"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        axis.title.x = element_blank(),
        text = element_text(family = "serif", size = 16),
        axis.text.x = element_text(hjust = 1, angle = 15))

# Saving
ggsave("../results/01_Postzygotics.png", plotA, device = "png", width = 180*2, height = 180, units = "mm", dpi = 600, family = "Times")

# Now for prezygotics
heterospecifics <- prezygotics

# Pasting generation and type of cross
heterospecifics$Type <- paste0(heterospecifics$Generation, "-", heterospecifics$Cross)

# Removing more columns
heterospecifics <- heterospecifics[,-c(2,3)]

# Factorying type
heterospecifics$Type <- gsub("F0-","", heterospecifics$Type)
heterospecifics$Type <- factor(heterospecifics$Type, levels = unique(heterospecifics$Type))

# Tidying
heterospecifics <- gather(heterospecifics, "Barrier", "Isolation", 2:6)

# Factoring Barriers
heterospecifics$Barrier <- factor(heterospecifics$Barrier, levels = unique(heterospecifics$Barrier))

# Replacing 0 by a tiny value (to increase a little bit the bar)
heterospecifics[heterospecifics==0] <- 0.01

# Plotting
plotA <- ggplot(heterospecifics) +
  geom_col(aes(x=Type, y=Isolation, fill=Year, color=Year), position = position_dodge()) +
  geom_hline(aes(yintercept=0))+
  ylim(NA, 1) +
  scale_fill_manual(values = c('#66c2a5','#8da0cb','#fc8d62')) +
  scale_color_manual(values = c('#66c2a5','#8da0cb','#fc8d62')) +
  labs(y="Absolute Reproductive Isolation") +
  facet_wrap(. ~ Barrier, ncol = 3, scales = "free", dir = "h") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "gray"),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        axis.title.x = element_blank(),
        text = element_text(family = "serif", size = 18))

# Saving
ggsave("../results/01_Prezygotics.png", plotA, device = "png", width = 180*1.75, height = 180, units = "mm", dpi = 600, family = "Times")
