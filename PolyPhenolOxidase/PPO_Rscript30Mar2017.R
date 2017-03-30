# Packages
library(ggplot2)
library(plyr)
library(reshape2)

setwd("~/Documents/Friesen lab/Enzymes copy/Methods_Paper/PolyPhenolOxidase/")

PPO_data <- read.csv("PPO_Micro_Spec_30Mar2017.csv")
PPO_data$Time <- factor(PPO_data$Time, levels = c("Before", "After"))

# Shawna
ggplot(PPO_data, aes(Tomato, StdAbs, fill = Time)) + geom_errorbar(aes(ymin = StdAbs- StdDev, ymax = StdAbs +StdDev), position = "dodge") + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ Method, scales = "free_y") + theme_classic() + scale_y_continuous(expand = c(0, 0)) + theme(strip.background = element_rect(colour = "white"), strip.text.x = element_text(size = 14)) + scale_fill_grey() +labs(x = "Tomato Replicate", y = "Absorbance standardized by weight") 

ggsave(file = "PPO_Microplate_Spec_Raw_Comp-March_30_2017.pdf")
