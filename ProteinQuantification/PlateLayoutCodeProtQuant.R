# 11/29/2016

# Making plate templates for Protein Quantification

# Packages Needed:
library(ggplot2)
library(dplyr)
library(ggplot2bdc)
library(cowplot)

# Making plate templates for Protein Quantification
setwd("~/Documents/Friesen\ lab/Enzymes\ copy/Methods_Paper/ProteinQuantification")

platemap1 <- read.csv("PlateMap1_11_29_2016.csv")
platemap1 <-subset(platemap1, SampleType !="")

platemap1 <- mutate(platemap1, Row=as.numeric(match(toupper(substr(Well, 1, 1)), LETTERS)), Column=as.numeric(substr(Well, 2, 5)))

p1 <-ggplot(data=platemap1, aes(x=Column, y=Row)) + geom_point(data=expand.grid(seq(1, 12), seq(1, 8)), aes(x=Var1, y=Var2), color="grey90", fill="white", shape=21, size=6) + geom_point(aes(shape = SampleType, colour = SampleType),size=8) +  coord_fixed(ratio=(13/12)/(9/8), xlim = c(0.5, 12.5), ylim=c(0.5, 8.5)) + scale_y_reverse(breaks=seq(1, 8), labels=LETTERS[1:8]) + scale_x_continuous(breaks=seq(1, 12)) + theme_bdc_microtiter() + geom_text(aes(label = SampLAb), size = 2.1) +scale_color_manual(values=c("Standard"="dodgerblue", "Experiment"="mediumspringgreen", "BLANK"="red")) + guides(shape = guide_legend(override.aes = list(size = 3)), color = guide_legend(override.aes = list(size = 3))) + labs(title=" Layout for Protein Quantification Plate 1 (12/1/16)")
ggsave(file = "ProteinPlateLabel1_12_1_16.pdf", height = 5, width = 8)

# 2nd plate
platemap2 <- read.csv("PlateMap2_11_29_2016.csv")
platemap2 <-subset(platemap2, SampleType !="")

platemap2 <- mutate(platemap2, Row=as.numeric(match(toupper(substr(Well, 1, 1)), LETTERS)), Column=as.numeric(substr(Well, 2, 5)))


p2 <-ggplot(data=platemap2, aes(x=Column, y=Row)) + geom_point(data=expand.grid(seq(1, 12), seq(1, 8)), aes(x=Var1, y=Var2), color="grey90", fill="white", shape=21, size=6) + geom_point(aes(shape = SampleType, colour = SampleType),size=8) +  coord_fixed(ratio=(13/12)/(9/8), xlim = c(0.5, 12.5), ylim=c(0.5, 8.5)) + scale_y_reverse(breaks=seq(1, 8), labels=LETTERS[1:8]) + scale_x_continuous(breaks=seq(1, 12)) + theme_bdc_microtiter() + geom_text(aes(label = SampLab), size = 2.1) +scale_color_manual(values=c("Standard"="dodgerblue", "Experiment"="mediumspringgreen", "BLANK"="red")) + guides(shape = guide_legend(override.aes = list(size = 3)), color = guide_legend(override.aes = list(size = 3))) + labs(title=" Layout for Protein Quantification Plate 2 (12/1/16)")
ggsave(file = "ProteinPlateLabel2_12_1_16.pdf", height = 5, width = 8)

pgrid <- plot_grid(p1, p2, nrow = 2)
save_plot("ProteinPlateLabel_12_1_16.pdf", pgrid, nrow = 2, base_aspect_ratio = 1.5)
