# Finally the plot of the big accumulative plots
rm(list = ls())
library(ggplot2)
library(tidyr)
library(rvg)
library(here)
library(officer)

# Figure options
# Text size in points
s <- 7

# Reading data
cumulative <- read.csv("../data/Cumulative.csv", header = T, encoding = "UTF-8")

# Tidying
cumulative <- gather(cumulative, "Barrier", "Isolation", 7:11)

# Factoring
cumulative$Barrier <- factor(cumulative$Barrier, levels = unique(cumulative$Barrier), labels = c("Mecánico", "Táctil", "Oviposición", "Fecunidad", "Fertilidad"))
pre <- cumulative[cumulative$Cross=="G♂E♀",]
pre$Barrier <- factor(pre$Barrier, levels = unique(pre$Barrier))
pre <- pre[pre$Year!="2019",]

# Prezygotic plots
png("../../../../Archivos/ECO-LOGICO/Barreras.png", width = 1200, height = 2400, res = 600)
ggplot(pre) +
          geom_line(aes(x=Barrier, y=Isolation, color=Year, group=Year)) +
          geom_point(aes(x=Barrier, y=Isolation, color=Year)) +
          scale_color_manual(values = rev(c("red","#984ea3"))) +
          theme_classic() +
          labs(y="Aislamiento reproductivo") +
          theme(axis.title.x = element_blank(),
                strip.background = element_blank(),
                strip.text = element_text(hjust = 0),
                text = element_text(family = "serif", size = s),
                axis.text.x = element_text(hjust = 1, angle = 15),
                legend.position = "none")

dev.off()
