# Methods Paper H2O2

setwd("~/Documents/Friesen lab/Enzymes copy/Methods_Paper/HydrogenPeroxide")
StdCurveH2 <- data.frame(Absorbance = c(0.0664,	0.082,	0.1341,	0.2193,	0.1899,	0.3097), H2O2 = c(0,	2.5,	5,	10,	15,	20))

fit <- lm(StdCurveH2$Absorbance ~ StdCurveH2$H2O2) 
summary(fit)
coef(fit)

ggplot(StdCurveH2, aes(x = H2O2, y = Absorbance)) + geom_point() + geom_smooth(method = "lm", se = F) + annotate("text", label = " y = 0.01128x + 0.06820", x = 15, y = 0.1) + annotate("text", label = "R^2 = 0.8729", x = 15, y = 0.07) + labs(x = "Concentration (ug/ml)", y = "Absorbance") + ggtitle("H2O2 Standard Curve 30 March 2017")
ggsave(file = "H2O2_std_curve_30mar2017.pdf")

H2 <- read.csv("H202_tomato_30March2017.csv")

H2$Time <- factor(H2$Time, levels = c("Before", "After"))
ggplot(H2, aes(Tomato, H2O2, fill = Time)) + geom_bar(stat = "identity", position = "dodge")  + ggtitle("H2O2 production") + labs(y = "H2O2 conc. \n standardized for plant tissue weight", x = "")  + scale_fill_grey(start = 0.25, end = 0.7) + scale_y_continuous(expand = c(0, 0))

ggsave(file = "H2O2_data_30mar2017.pdf")
