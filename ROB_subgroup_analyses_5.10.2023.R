library(readxl)
library(tidyverse)
library(readxl)
library(ggpubr)
library(extrafont)
library(cowplot)
library(grid)


#Overweight Low Bias
BMI_overweight_table_for_meta_low <- read_excel("Rob sub-groups/BMI_overweight_table_for_meta_low.xlsx")
se <- meta:::TE.seTE.ci(log(BMI_overweight_table_for_meta_low$LCI), log(BMI_overweight_table_for_meta_low$UCI))$seTE; se
str(BMI_overweight_table_for_meta_low)
bmi_meta_overweight_low <- metagen(log(OR), SE, studlab = paste(Authors, "Publication Year"),data=BMI_overweight_table_for_meta_low, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_overweight_low$studlab <- BMI_overweight_table_for_meta_low$Authors
##graph
forest(bmi_meta_overweight_low,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "l",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_overweight_low, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_meta_overweight_low,
  se,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)

#Overweight Moderate Bias
BMI_overweight_table_for_meta_moderate <- read_excel("Rob sub-groups/BMI_overweight_table_for_meta_moderate.xlsx")
se.ov.mod <- meta:::TE.seTE.ci(log(BMI_overweight_table_for_meta_moderate$LCI), log(BMI_overweight_table_for_meta_moderate$UCI))$seTE; se.ov.mod
str(BMI_overweight_table_for_meta_moderate)
bmi_meta_overweight_moderate <- metagen(log(OR), SE, studlab = paste(Authors, "Publication Year"),data=BMI_overweight_table_for_meta_moderate, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_overweight_moderate$studlab <- BMI_overweight_table_for_meta_moderate$Authors

##graph
forest(bmi_meta_overweight_moderate,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "l",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_overweight_moderate, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_meta_overweight_moderate,
  se.ov.mod,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)


#Overweight Serious Bias
BMI_overweight_table_for_meta_serious <- read_excel("Rob sub-groups/BMI_overweight_table_for_meta_serious.xlsx")
se.ov.ser <- meta:::TE.seTE.ci(log(BMI_overweight_table_for_meta_serious$LCI), log(BMI_overweight_table_for_meta_serious$UCI))$seTE; se.ov.mod
str(BMI_overweight_table_for_meta_serious)
bmi_meta_overweight_serious <- metagen(log(OR), SE, studlab = paste(Authors, "Publication Year"),data=BMI_overweight_table_for_meta_serious, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_overweight_serious$studlab <- BMI_overweight_table_for_meta_serious$Authors

##graph
forest(bmi_meta_overweight_serious,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "l",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_overweight_serious, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_meta_overweight_serious,
  se.ov.ser,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)


#Obese Low Bias
BMI_obese_table_for_meta_low <- read_excel("Rob sub-groups/BMI_obese_table_for_meta2.2_low.xlsx")
se.ob.low <- meta:::TE.seTE.ci(log(BMI_obese_table_for_meta_low$LCI), log(BMI_obese_table_for_meta_low$UCI))$seTE; se.ob.low
str(BMI_obese_table_for_meta_low)
bmi_meta_obese_low <- metagen(log(OR), SE, studlab = paste(Authors, "Publication Year"),data=BMI_obese_table_for_meta_low, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_obese_low$studlab <- BMI_obese_table_for_meta_low$Authors
##graph
forest(bmi_meta_obese_low,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "l",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_obese_low, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_meta_obese_low,
  se.ob.low,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)


#Obese moderate Bias
BMI_obese_table_for_meta_moderate <- read_excel("Rob sub-groups/BMI_obese_table_for_meta2.2_moderate.xlsx")
se.ob.moderate <- meta:::TE.seTE.ci(log(BMI_obese_table_for_meta_moderate$LCI), log(BMI_obese_table_for_meta_moderate$UCI))$seTE; se.ob.moderate
str(BMI_obese_table_for_meta_moderate)
bmi_meta_obese_moderate <- metagen(log(OR), SE, studlab = paste(Authors, "Publication Year"),data=BMI_obese_table_for_meta_moderate, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_obese_moderate$studlab <- BMI_obese_table_for_meta_moderate$Authors
##graph
forest(bmi_meta_obese_moderate,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "l",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_obese_moderate, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_meta_obese_moderate,
  se.ob.moderate,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)


#Obese serious Bias
BMI_obese_table_for_meta_serious <- read_excel("Rob sub-groups/BMI_obese_table_for_meta2.2_serious.xlsx")
se.ob.serious <- meta:::TE.seTE.ci(log(BMI_obese_table_for_meta_serious$LCI), log(BMI_obese_table_for_meta_serious$UCI))$seTE; se.ob.serious
str(BMI_obese_table_for_meta_serious)
bmi_meta_obese_serious <- metagen(log(OR), SE, studlab = paste(Authors, "Publication Year"),data=BMI_obese_table_for_meta_serious, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_obese_serious$studlab <- BMI_obese_table_for_meta_serious$Authors
##graph
forest(bmi_meta_obese_serious,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "l",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_obese_serious, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_meta_obese_serious,
  se.ob.serious,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)

#Continuous Low Bias
BMI_continuous_table_for_meta_low <- read_excel("Rob sub-groups/BMI_rcd_table_for_meta_continous_low.xlsx")
se.cont.low <- meta:::TE.seTE.ci(log(BMI_continuous_table_for_meta_low$LCI), log(BMI_continuous_table_for_meta_low$UCI))$seTE; se.cont.low
str(BMI_continuous_table_for_meta_low)
bmi_meta_continuous_low <- metagen(log(OR), SE, studlab = paste(Authors, "Publication Year"),data=BMI_continuous_table_for_meta_low, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_continuous_low$studlab <- BMI_continuous_table_for_meta_low$Authors
##graph
forest(bmi_meta_continuous_low,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "l",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_continuous_low, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_meta_continuous_low,
  #se.cont.low,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)


#Continuous moderate Bias
BMI_continuous_table_for_meta_moderate <- read_excel("Rob sub-groups/BMI_rcd_table_for_meta_continous_moderate.xlsx")
se.cont.moderate <- meta:::TE.seTE.ci(log(BMI_continuous_table_for_meta_moderate$LCI), log(BMI_continuous_table_for_meta_moderate$UCI))$seTE; se.cont.moderate
str(BMI_continuous_table_for_meta_moderate)
bmi_meta_continuous_moderate <- metagen(log(OR), SE, studlab = paste(Authors, "Publication Year"),data=BMI_continuous_table_for_meta_moderate, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_continuous_moderate$studlab <- BMI_continuous_table_for_meta_moderate$Authors
##graph
forest(bmi_meta_continuous_moderate,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "l",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_continuous_moderate, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_meta_continuous_moderate,
  se.cont.moderate,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)


#Continuous serious Bias
BMI_continuous_table_for_meta_serious <- read_excel("Rob sub-groups/BMI_rcd_table_for_meta_continous_serious.xlsx")
se.cont.serious <- meta:::TE.seTE.ci(log(BMI_continuous_table_for_meta_serious$LCI), log(BMI_continuous_table_for_meta_serious$UCI))$seTE; se.cont.serious
str(BMI_continuous_table_for_meta_serious)
bmi_meta_continuous_serious <- metagen(log(OR), SE, studlab = paste(Authors, "Publication Year"),data=BMI_continuous_table_for_meta_serious, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_continuous_serious$studlab <- BMI_continuous_table_for_meta_serious$Authors
##graph
forest(bmi_meta_continuous_serious,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "l",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_continuous_serious, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_meta_continuous_serious,
  se.cont.serious,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)

# Mean Difference Low
BMI_meta_means_table_low <- read_excel("Rob sub-groups/BMI_meta_means_table_low.xlsx")
se.means.low <- meta:::TE.seTE.ci(BMI_meta_means_table_low$LCI, BMI_meta_means_table_low$UCI)$seTE; se.means.low
bmi_means_low <- metacont(n.e = BMI_meta_means_table_low$Cases, mean.e = BMI_meta_means_table_low$Cases_BMI_Mean , sd.e = BMI_meta_means_table_low$Cases_BMI_SD, n.c= BMI_meta_means_table_low$Controls, mean.c= BMI_meta_means_table_low$Controls_BMI_Mean, sd.c= BMI_meta_means_table_low$Controls_BMI_SD, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "MD" )
bmi_means_low$studlab <- BMI_meta_means_table_low$Authors

#graphs

forest(bmi_means_low,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")
funnel(bmi_means_low, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_means_low,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)


# Mean Difference Moderate
BMI_meta_means_table_moderate <- read_excel("Rob sub-groups/BMI_meta_means_table_moderate.xlsx")
se.means.moderate <- meta:::TE.seTE.ci(BMI_meta_means_table_moderate$LCI, BMI_meta_means_table_moderate$UCI)$seTE; se.means.moderate
bmi_means_moderate <- metacont(n.e = BMI_meta_means_table_moderate$Cases, mean.e = BMI_meta_means_table_moderate$Cases_BMI_Mean , sd.e = BMI_meta_means_table_moderate$Cases_BMI_SD, n.c= BMI_meta_means_table_moderate$Controls, mean.c= BMI_meta_means_table_moderate$Controls_BMI_Mean, sd.c= BMI_meta_means_table_moderate$Controls_BMI_SD, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "MD" )
bmi_means_moderate$studlab <- BMI_meta_means_table_moderate$Authors

#graphs

forest(bmi_means_moderate,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")
funnel(bmi_means_moderate, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_means_moderate,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)

# Mean Difference serious
BMI_meta_means_table_serious <- read_excel("Rob sub-groups/BMI_meta_means_table_serious.xlsx")
se.means.serious <- meta:::TE.seTE.ci(BMI_meta_means_table_serious$LCI, BMI_meta_means_table_serious$UCI)$seTE; se.means.serious
bmi_means_serious <- metacont(n.e = BMI_meta_means_table_serious$Cases, mean.e = BMI_meta_means_table_serious$Cases_BMI_Mean , sd.e = BMI_meta_means_table_serious$Cases_BMI_SD, n.c= BMI_meta_means_table_serious$Controls, mean.c= BMI_meta_means_table_serious$Controls_BMI_Mean, sd.c= BMI_meta_means_table_serious$Controls_BMI_SD, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "MD" )
bmi_means_serious$studlab <- BMI_meta_means_table_serious$Authors

#graphs

forest(bmi_means_serious,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")
funnel(bmi_means_serious, 
       xlab = "Odds Ratio", 
       ylab = "Standard Error of log Odds Ratio",
       col = "black", 
       bg = "black",
       #bg is for the symbols background color
       level = 0.95, 
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("grey40", "grey60", "gray85"),
       lwd = 2, 
       pch = 16) 
legend("topright",  
       cex = 0.8,
       c("0.10 < p ≤ 1.00","0.05 < p ≤ 0.10", "0.01 < p ≤ 0.05", "0.00 < p ≤ 0.01", "Studies"),
       col = c("black","grey40","grey60", "grey85","black" ), 
       pch = c(0,15,15,15,16), 
       pt.cex = 1.5,
       x.intersp = 0.70,
       inset= c(0.01, 0.01))
metabias(
  bmi_means_serious,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)



