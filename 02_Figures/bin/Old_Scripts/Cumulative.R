# Finally the plot of the big accumulative plots
rm(list = ls())
library(ggplot2)
library(tidyr)
library(gridExtra)

# Reading data
cumulative <- read.csv("../data/Cumulative.csv", header = T, encoding = "UTF-8")

# Tidying
cumulative <- gather(cumulative, "Barrier", "Isolation", 7:11)

# Changing second barrier name
cumulative[cumulative=="Tactile"] <- "Mechanical-Tactile"

# Factoring
cumulative$Barrier <- factor(cumulative$Barrier, levels = unique(cumulative$Barrier))

# Replacing colors
cumulative[cumulative$Color=="A","Color"] <- "#377eb8"
cumulative[cumulative$Color=="B","Color"] <- "#4daf4a"
cumulative[cumulative$Color=="C","Color"] <- "#984ea3"

# Sorting by Cross
cumulative <- cumulative[order(cumulative$Cross),]

# Plotting within a for loop
for (i in LETTERS[1:21]) {
  plot <- ggplot(cumulative[cumulative$Group==i,]) +
          geom_hline(aes(yintercept=0)) +
          geom_line(aes(x=Barrier, y=Isolation, color=Cross, group=Type)) +
          geom_point(aes(x=Barrier, y=Isolation, color=Cross)) +
          scale_color_manual(values=unique(cumulative[cumulative$Group==i,"Color"])) +
          scale_y_continuous(limits = c(NA, 1), n.breaks = 7) +
          labs(title=paste0(i)) + 
          theme_classic() +
          theme(axis.title = element_blank(),
                text = element_text(family = "serif"),
                axis.text.x = element_text(size = 7, angle = 15, hjust = 1),
                legend.position="top",
                legend.margin=margin(0,0,0,0),
                legend.box.margin=margin(-10,-10,-10,-10),
                legend.title = element_blank())
  assign(paste0("Plot",i), plot)
}
rm(plot)

# Gridding
final.plot <- grid.arrange(PlotA, PlotB, PlotC, PlotD, PlotE, PlotF, PlotG, PlotH, PlotI, PlotJ, PlotK, PlotL, PlotM, PlotN, PlotO, PlotP, PlotQ, PlotR, PlotS, PlotT, PlotU,
             layout_matrix = rbind(c( 1,  2, NA, NA, NA, NA),
                                   c( 3,  4,  5, NA, NA, NA),
                                   c( 6,  7, NA,  8,  9, NA),
                                   c(10, 11, 12, 13, 14, 15),
                                   c(16, 17, 18, 19, 20, 21)))
dev.off()

# Saving
ggsave("../results/02_Cumulative.png", final.plot, device = "png", width = 180*2, height = 180*1.5, units = "mm", dpi = 600, family = "Times")
