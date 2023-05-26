#Meta-Analysis of BMI and RCD 09/06/2022 ##by Simone Herzberg

setwd("~/Desktop/Simone/Graduate school/Rotator cuff /Systematic Review")
#install.packages("meta")
library(meta)
settings.meta(CIseparator = "-", CIbracket = "(")

bmi_meta_1 <- read.csv("BMI_overweight_table_for_meta.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_2 <- read.csv("BMI_obese_table_for_meta.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_2.2 <- read.csv("BMI_obese_table_for_meta2.2.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_2.3 <- read.csv("BMI_obese_table_for_meta2.3.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_2.rct <- read.csv("BMI_obese_rct_table_for_meta.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_2.2rct <- read.csv("BMI_obese_rct_table_for_meta2.2.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_2.3rct <- read.csv("BMI_obese_rct_table_for_meta2.3.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_3 <- read.csv("BMI_rcd_table_for_meta.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_3.5 <- read.csv("BMI_rcd_table_for_meta_5.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_4 <- read.csv("BMI_rct_table_for_meta.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_4.5 <- read.csv("BMI_rct_table_for_meta5.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_5 <- read.csv("BMI_meta_means_table.csv", stringsAsFactors = FALSE, header = T)
bmi_meta_5_rct <- read.csv("BMI_meta_means_table_rct.csv", stringsAsFactors = FALSE, header = T)

bmi_meta_IVPTW_SEO<- read.csv("bmi_meta_seo.csv", stringsAsFactors = FALSE, header = T)

se <- meta:::TE.seTE.ci(log(bmi_meta_1$LCI), log(bmi_meta_1$UCI))$seTE; se
se2 <- meta:::TE.seTE.ci(log(bmi_meta_2$LCI), log(bmi_meta_2$UCI))$seTE; se2
se2.2 <- meta:::TE.seTE.ci(log(bmi_meta_2.2$LCI), log(bmi_meta_2.2$UCI))$seTE; se2.2
se2.3 <- meta:::TE.seTE.ci(log(bmi_meta_2.3$LCI), log(bmi_meta_2.2$UCI))$seTE; se2.2
se2.rct <- meta:::TE.seTE.ci(log(bmi_meta_2.rct$LCI), log(bmi_meta_2.rct$UCI))$seTE; se2.2
se2.2rct <- meta:::TE.seTE.ci(log(bmi_meta_2.2rct$LCI), log(bmi_meta_2.2rct$UCI))$seTE; se2.2
se2.3rct <- meta:::TE.seTE.ci(log(bmi_meta_2.3rct$LCI), log(bmi_meta_2.2rct$UCI))$seTE; se2.2
se3 <- meta:::TE.seTE.ci(log(bmi_meta_3$LCI), log(bmi_meta_3$UCI))$seTE; se3
se3.5 <- meta:::TE.seTE.ci(log(bmi_meta_3.5$LCI), log(bmi_meta_3.5$UCI))$seTE; se3.5
se4 <- meta:::TE.seTE.ci(log(bmi_meta_4$LCI), log(bmi_meta_4$UCI))$seTE; se4
se4.5 <- meta:::TE.seTE.ci(log(bmi_meta_4.5$LCI), log(bmi_meta_4.5$UCI))$seTE; se4.5
se5 <- meta:::TE.seTE.ci(bmi_meta_5$LCI, bmi_meta_5$UCI)$seTE; se5
se5rct <- meta:::TE.seTE.ci(bmi_meta_5_rct$LCI, bmi_meta_5_rct$UCI)$seTE; se5
seseo <- meta:::TE.seTE.ci(log(bmi_meta_IVPTW_SEO$LCI), log(bmi_meta_IVPTW_SEO$UCI))$seTE; seseo

bmi_meta_1$lower <- round(exp(ci(TE=log(bmi_meta_1$OR), seTE=se)$lower), 3)
bmi_meta_1$upper <- round(exp(ci(TE=log(bmi_meta_1$OR), seTE=se)$upper), 3)
bmi_meta_1

str(bmi_meta_1)
str(bmi_meta_2)
str(bmi_meta_2.2)
str(bmi_meta_2.2)
str(bmi_meta_2.3)
str(bmi_meta_2.rct)
str(bmi_meta_2.2rct)
str(bmi_meta_2.3rct)
str(bmi_meta_3)
str(bmi_meta_3.5)
str(bmi_meta_4)
str(bmi_meta_4.5)


bmi_meta_overweight <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_1, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_obese <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_2, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_obese2.2 <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_2.2, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_obese2.3 <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_2.3, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_obese_rct <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_2.rct, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_obese_rct2.2 <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_2.2rct, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_obese_rct2.3 <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_2.3rct, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_rcd <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_3, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_rcd5 <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_3.5, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_rct <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_4, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_rct5 <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_4.5, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )
bmi_meta_means <- metacont(n.e = bmi_meta_5$Cases, mean.e = bmi_meta_5$Cases_BMI_Mean , sd.e = bmi_meta_5$Cases_BMI_SD, n.c= bmi_meta_5$Controls, mean.c= bmi_meta_5$Controls_BMI_Mean, sd.c= bmi_meta_5$Controls_BMI_SD, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "MD" )
bmi_meta_means_rct <- metacont(n.e = bmi_meta_5_rct$Cases, mean.e = bmi_meta_5_rct$Cases_BMI_Mean , sd.e = bmi_meta_5_rct$Cases_BMI_SD, n.c= bmi_meta_5_rct$Controls, mean.c= bmi_meta_5_rct$Controls_BMI_Mean, sd.c= bmi_meta_5_rct$Controls_BMI_SD, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "MD" )
bmi_meta_means2 <- metagen(TE= bmi_meta_5$Mean.Difference, seTE = se5, studlab = paste(Authors, Publication.Year),data=bmi_meta_5, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "MD" )
bmi_meta_means_rct2 <- metagen(TE= bmi_meta_5_rct$Mean.Difference, seTE = se5rct, studlab = paste(Authors, Publication.Year),data=bmi_meta_5_rct, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "MD" )


bmi_meta_seo <- metagen(log(OR), SE, studlab = paste(Authors, Publication.Year),data=bmi_meta_IVPTW_SEO, comb.fixed = TRUE, comb.random = FALSE, prediction = TRUE, sm = "OR" )

bmi_meta_overweight$studlab <- bmi_meta_1$Authors
bmi_meta_obese$studlab <- bmi_meta_2$Authors
bmi_meta_obese2.2$studlab <- bmi_meta_2.2$Authors
bmi_meta_obese2.3$studlab <- bmi_meta_2.3$Authors
bmi_meta_obese_rct$studlab <- bmi_meta_2.rct$Authors
bmi_meta_obese_rct2.2$studlab <- bmi_meta_2.2rct$Authors
bmi_meta_obese_rct2.3$studlab <- bmi_meta_2.3rct$Authors
bmi_meta_rcd$studlab <- bmi_meta_3$Authors
bmi_meta_rcd5$studlab <- bmi_meta_3.5$Authors
bmi_meta_rct$studlab <- bmi_meta_4$Authors
bmi_meta_rct5$studlab <- bmi_meta_4.5$Authors
bmi_meta_means$studlab <- bmi_meta_5$Authors
bmi_meta_means_rct$studlab <- bmi_meta_5_rct$Authors
bmi_meta_means2$studlab <- bmi_meta_5$Authors
bmi_meta_means_rct2$studlab <- bmi_meta_5_rct$Authors
bmi_meta_seo$studlab <- bmi_meta_IVPTW_SEO$Authors
## Graphs 

#Overweight Categorical all studies:

forest(bmi_meta_overweight,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication.Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "l",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_overweight, 
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
  bmi_meta_overweight,
  se,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)

## Obese catagorical all 
forest(bmi_meta_obese2.2,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication.Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "c",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_obese2.2, 
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
  bmi_meta_obese2.2,
  se2.2,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)
#Obese catagorical  RCT tear only
forest(bmi_meta_obese_rct2.2,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication.Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "c",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_obese_rct2.2, 
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
  bmi_meta_obese_rct2.2,
  se2.2rct,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)
## Continous BMI RCD by 5 units BMI
forest(bmi_meta_rcd5,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication.Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "c",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")
funnel(bmi_meta_rcd5, 
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
  bmi_meta_rcd5,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)

## Continuous BMI RCT 5 unit BMI


forest(bmi_meta_rct5,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication.Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "c",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_rct5, 
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
  bmi_meta_rct5,
  se4.5,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)

# Mean difference comparison

forest(bmi_meta_means2,
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
funnel(bmi_meta_means2, 
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
  bmi_meta_means2,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)

#meta means rct
forest(bmi_meta_means_rct,
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
funnel(bmi_meta_means_rct, 
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
  bmi_meta_means_rct2,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 1,)


#Other COdes
forest(bmi_meta_obese,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication.Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "c",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")
metabias(
  bmi_meta_obese,
  se2,
  method.bias = "Egger",
  plotit = FALSE,
  correct = FALSE,
  k.min = 10,)

funnel(bmi_meta_obese, 
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


forest(bmi_meta_obese2.3,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication.Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "c",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")


funnel(bmi_meta_obese2.3, 
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

forest(bmi_meta_obese_rct,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication.Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "c",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

funnel(bmi_meta_obese_rct, 
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

forest(bmi_meta_obese_rct2.3,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication.Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "c",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")

forest(bmi_meta_rcd,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication.Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "c",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")




funnel(bmi_meta_rcd, 
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


funnel(bmi_meta_obese_rct2.3, 
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


forest(bmi_meta_rct,
       sortvar = TE,
       comb.random = FALSE,
       comb.fixed = TRUE,
       text.fixed = "Overall effect",
       prediction = TRUE,
       text.predict = "95% PI",
       print.tau2 = FALSE,
       backtransf = TRUE,
       leftcols = c("Authors","Publication.Year"), 
       leftlabs = c("Author", "Publication Year"), 
       rightcols = c("effect.ci", "w.fixed"),
       rightlabs = c("ES(95% CI)", "Weight"),
       just = "l", just.addcols.left = "c",
       just.addcols.right ="l", just.studlab = "l", 
       col.square = "green",
       col.diamond = "black", 
       col.predict = "red")


funnel(bmi_meta_rct, 
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

forest(bmi_meta_seo)

