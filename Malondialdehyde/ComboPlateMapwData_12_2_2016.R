# Dec 2, 2016

# Steps to making color block of plates

# Malondialdehyde Assay for Methods Paper

library(ggplot2)
library(platetools)
library(plater)
library(dplyr)

setwd("~/Documents/Friesen\ lab/Enzymes\ copy/Methods_Paper/Malondialdehyde")

# If my plate map and my data file had the same number of entries, I could use the plater package. However, for the MDA plate map, there are skipped wells. Because I ran the whole plate on the spec, I have values for those wells in the data file, which will throw an error. Below is a work-around.

# I copied the data from the 4th floor spec in plate format, using the plater package, I convert that matrix to a dataframe.

# The malondialdehyde assay is scanned at two wavelengths: 532 and 600 so I need both datasets. The plate data for each are placed vertically with an empty row between them. See MDA_PlaterForm.csv

data <- read_plate(file = "MDA_PlaterForm.csv", well_ids_column = "Well")
head(data)

# Getting my plate map. I never saved the subsetted version of the plate, so the second line is needed.
platemap1 <- read.csv("MDA_PlateMap_12_1_2016.csv")
platemap1 <-subset(platemap1, SampleType !="")
head(platemap1)

# Combining the two csv files.
MDA_dataset <- inner_join(platemap1, data, by = "Well")
head(MDA_dataset)

# This is all that is needed to make plate data maps, but we will need to add weight data later.

with(MDA_dataset, raw_map(data = ABS532, well = Well, plate = 96) + scale_fill_gradient(limits = c(0,0.5), low = "white", high = "maroon1", guide = guide_legend(title = expression(Abs[532]))) + geom_text(aes(label = MDA_dataset$SampLab), size = 2.1)) + theme(panel.grid = element_blank())

ggsave(file = "Uncorrected_MDA_532nm_12_2_2016.pdf")

with(MDA_dataset, raw_map(data = ABS600, well = Well, plate = 96)+ scale_fill_gradient(limits = c(0,0.5), low = "white", high = "maroon1", guide = guide_legend(title = expression(Abs[600]))) + geom_text(aes(label = MDA_dataset$SampLab), size = 2.1)) + theme(panel.grid = element_blank())

ggsave(file = "Uncorrected_MDA_600nm_12_2_2016.pdf")

# Subtracting 600nm Abs from 532nm Abs
MDA_dataset <- within(MDA_dataset, CorrectedAbs <- ABS532-ABS600)
head(MDA_dataset)
max(MDA_dataset$CorrectedAbs)

with(MDA_dataset, raw_map(data = CorrectedAbs, well = Well, plate = 96) + scale_fill_gradient(limits = c(0,0.5), low = "white", high = "maroon1", guide = guide_legend(title = "Corrected Abs")) + geom_text(aes(label = MDA_dataset$SampLab), size = 2.1)) + theme(panel.grid = element_blank())

ggsave(file = "Corrected_MDA_12_2_2016.pdf")
