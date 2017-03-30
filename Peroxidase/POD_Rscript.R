# Packages
library(ggplot2)
library(plyr)
library(reshape2)

setwd("~/Documents/Friesen lab/Enzymes copy/Methods_Paper/Peroxidase/")

POD_data <- read.csv("Micro_Spec_Combo_3Feb2017.csv")
POD_data$Time <- factor(POD_data$Time, levels = c("Before", "After"))

# Shawna
ggplot(POD_data, aes(Tomato, StdAbs, fill = Time)) + geom_errorbar(aes(ymin = StdAbs- StdDev, ymax = StdAbs +StdDev), position = "dodge") + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ Method, scales = "free_y") + theme_classic() + scale_y_continuous(expand = c(0, 0)) + theme(strip.background = element_rect(colour = "white"), strip.text.x = element_text(size = 14)) + scale_fill_grey() +labs(x = "Tomato Replicate", y = "Absorbance standardized by weight") 
ggsave(file = "Microplate_Spec_Raw_Comp-March_30_2017.pdf")

POD_wide <- dcast(POD_data, Tomato + Method ~ Time, value.var = "StdAbs")
POD_wide$fold <- POD_wide$After/POD_wide$Before

ggplot(POD_wide, aes(Tomato, fold, fill = Method)) + geom_bar(stat = "identity", position = "dodge") + theme_classic() + scale_fill_grey() + scale_y_continuous(expand = c(0,0)) + labs(x = "Tomato Replicate", y = "Fold change")
ggsave(file = "Microplate_Spec_Fold_Comp-3Feb2017.pdf")
