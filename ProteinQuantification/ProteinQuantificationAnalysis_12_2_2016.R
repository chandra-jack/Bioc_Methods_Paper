# Dec 2, 2016

# Steps to making color block of plates

# Protein Quantification Assay for Methods Paper

library(ggplot2)
library(platetools)
library(plater)
library(dplyr)


setwd("~/Documents/Friesen\ lab/Enzymes\ copy/Methods_Paper/ProteinQuantification")

# If my plate map and my data file had the same number of entries, I could use the plater package. However, for the plate map, there are skipped wells. Because I ran the whole plate on the spec, I have values for those wells in the data file, which will throw an error. Below is a work-around.

# I copied the data from the 4th floor spec in plate format, using the plater package, I convert that matrix to a dataframe.

# This assay takes two plates, that were plate mapped separately, will need to combine them before combining with the data.

platemap1 <- read.csv("PlateMap1_11_29_2016.csv")
platemap1 <-subset(platemap1, SampleType !="")
platemap1$Plate <- "Plate1"

platemap2 <- read.csv("PlateMap2_11_29_2016.csv")
platemap2 <-subset(platemap2, SampleType !="")
platemap2$Plate <- "Plate2"


ComboPlate <- rbind(platemap1, platemap2)

# Combining data files
PEdata <- read_plates(files = c("ProteinQuantification_1_12_1_16.csv", "ProteinQuantification_2_12_1_16.csv"), plate_names = c("Plate1", "Plate2"), well_ids_column = "Well")

# Combining the two csv files.
Prot_Quan_dataset <- inner_join(ComboPlate, PEdata, by = c("Well", "Plate"))

# Making plate map with data
with(Prot_Quan_dataset, raw_grid(data = values, well = Well, plate_id = Plate, plate = 96)+ scale_fill_gradient(limits = c(0,2.5), low = "white", high = "purple", guide = guide_legend(title = expression(Abs[562]))) + geom_text(aes(label = Prot_Quan_dataset$SampLab), size = 2.1)) + theme(panel.grid = element_blank()) 
max(Prot_Quan_dataset$values)

ggsave(file = "Prot_Quan_12_2_2016.pdf", width = 16)

# Making Standard Curve
ProteinCurve <- subset(Prot_Quan_dataset, SampleType != "Experiment")
ProteinCurve$Sample <- as.numeric(levels(ProteinCurve$Sample))[ProteinCurve$Sample]
ggplot(ProteinCurve, aes(x = Sample, y = values)) + geom_point() + geom_smooth(method = "lm", se = F) + annotate("text", label = " y = 0.00103x + 0.0254", x = 250, y = 2) + annotate("text", label = "R^2 = 0.962", x = 250, y = 1.75) + labs(x = "Concentration (ug/ml)", y = "Absorbance") + ggtitle("Protein Standard Curve 12/1/2016")
fit <- lm(ProteinCurve$values ~ ProteinCurve$Sample) 
summary(fit)
coef(fit)


# Prepping leaf weight dataset

Leaf <- read.csv("~/Documents/Friesen\ lab/Enzymes\ copy/Methods_Paper/LeafDataWeight.csv")
Leaf$Time <- as.factor(Leaf$Time)

# Removing set 2
Leaf <- subset(Leaf, Set == 1)
Leaf <- subset(Leaf, Treatment == "Protein")

# Rename Time factor levels
levels(Leaf$Time)[match("0",levels(Leaf$Time))] <- "Before"
levels(Leaf$Time)[match("24",levels(Leaf$Time))] <- "After"
levels(Leaf$Species)[match("MP",levels(Leaf$Species))] <- "M_poly"
levels(Leaf$Species)[match("MT",levels(Leaf$Species))] <- "M_trun"

names(Leaf)[names(Leaf)=="Species"] <- "Sample"
names(Leaf)[names(Leaf)=="PlantNumber"] <- "Plant"

head(Leaf)


# Combining
LeafCombo <- inner_join(Leaf, Prot_Quan_dataset, by = c("Plant", "Sample", "Time"))

head(LeafCombo)
LeafCombo <- within(LeafCombo, Concentration <- ((values - 0.025402243)/ 0.001034718))

with(LeafCombo, raw_grid(data = Concentration, well = Well, plate_id = Plate, plate = 96)+ scale_fill_gradient(limits = c(0,500), low = "white", high = "purple", guide = guide_legend(title = expression(Abs[562])))  + geom_text(aes(label = LeafCombo$SampLab), size = 2.1)) + theme(panel.grid = element_blank())

LeafCombo <- within(LeafCombo, Protein <- ((Concentration * 10) / Weight))

with(LeafCombo, raw_grid(data = Protein, well = Well, plate_id = Plate, plate = 96)+ scale_fill_gradient(limits = c(0,160000), low = "white", high = "purple", guide = guide_legend(title = "Protein \n(ug/ml plant tissue)", size = 6))  + geom_text(aes(label = LeafCombo$SampLab), size = 2.1)) + theme(panel.grid = element_blank(), legend.title = element_text(size = 8)) 

ggsave(file = "Prot_Quan_Final_Map_12_2_2016.pdf", width = 16)

LeafCombo_Rep <- ddply(LeafCombo, c("Sample", "Plant", "Time"), summarise, mean_protein = mean(Protein), se_protein = sqrt(var(Protein)/ length(Protein)))

#Shawna
LeafCombo_Rep$Time <- factor(LeafCombo_Rep$Time, levels = c("Before", "After"))
ggplot(subset(LeafCombo_Rep, Sample == "Tomato"), aes(Plant, mean_protein, fill = Time)) + geom_bar(stat = "identity", position = "dodge") + geom_errorbar(aes(ymax = mean_protein + se_protein, ymin = mean_protein - se_protein), position = "dodge") + ggtitle("Protein Quantification 12/1/2016") + labs(y = "Protein \n(ug/ml plant tissue)", x = "")  + scale_fill_grey(start = 0.25, end = 0.7) + scale_y_continuous(expand = c(0, 0))
ggsave(file = "ProtQuantTechRep_March_30_2017.pdf")

LeafCombo_Rep2 <- ddply(LeafCombo, c("Sample", "Time"), summarise, mean_protein = mean(Protein), se_protein = sqrt(var(Protein)/ length(Protein)))

ggplot(LeafCombo_Rep2, aes(Sample, mean_protein, fill = Time)) + geom_bar(stat = "identity", position = "dodge") + geom_errorbar(aes(ymax = mean_protein + se_protein, ymin = mean_protein - se_protein), position = "dodge") + ggtitle("Protein Quantification 12/1/2016") + labs(y = "Protein \n(ug/ml plant tissue)", x = "Plant Species")
ggsave(file = "ProtQuantPlantRep_12_1_2016.pdf")
