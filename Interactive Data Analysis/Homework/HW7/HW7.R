library(dplyr)
library(outliers)
library(naniar)
library(MASS)
library(mice)
library(ggplot2)
library(EnvStats)

rm(list=ls())
setwd("C:/Users/Duker/Desktop/Fall 2020/CS 614/Homework/HW7")
load("HW7.Rdata")

# Problem 1 ####
attach(demo)

# Boxplot
boxplot(Prof_Score, data = demo)
bx.o = boxplot.stats(Prof_Score)$out

# Z-score
ol_SD = function(z){
  z = na.omit(z)
  z.score = scale(z)
  w = which(abs(z.score) > 3)
  return(z[w])
}

z.o = ol_SD(Prof_Score)

# Rosner Test 
out = rosnerTest(Prof_Score, 10)
out$n.outliers

length(bx.o)
length(z.o)

# The boxplot method detects almost 3 times as many outliers as the z-score method. The Rosner Test
# only shows 6 outliers.
# The discrepancy in outliers detected between the boxplot & z-score methods was justifyable since
# the boxplot's threshold is the median & z-score's threshold is the mean.
# I would think it would require ether more domain knowledge of the data or better visualization
# to approach outliers with the Rosner Test since you do have to start with an arbitrary "k" outliers.

paste("The outliers detected by the boxplot range from", round(min(bx.o),3), "to",
      round(max(bx.o),3))

paste("The outliers detected by the z-score method range from", round(min(z.o),3), "to",
      round(max(z.o),3))

# I would treat future outliers depending on the distribution of the data. If the data has high 
# variance, I would maybe approach outliers using the z-score. The reason is if the outliers are
# clustered together, the boxplot may not be able to detect them.

# Problem 2 ####
summary(df)
vis_miss(df)

IQ_mis = factor(ifelse(is.na(df$Image_Quality), 1, 0))
smile_mis = factor(ifelse(is.na(df$Smile), 1, 0))

chisq.test(df$Image_Type, IQ_mis)
chisq.test(df$Image_Color, smile_mis)

# There is statistical significance from Chi-Sq Test when comparing NA's from Image Quality &
# Smile vs. Image Type & Image Color. Thus, the NA's can be classified as MAR.

# List-wise deletion
lw.df = na.omit(df)

# Imputation
imp = mice(df, maxit = 5, print =  FALSE)

impute.df = complete(imp, method = logreg, include = F)


lin.reg1 = lm(Area~., data = lw.df)
final1 = stepAIC(lin.reg1, trace = 0)
summary(final1)

lin.reg2 = lm(Area~., data = impute.df)
final2 = stepAIC(lin.reg2, trace = 0)
summary(final2)

# There isn't a significant difference between the R-squared scores from the 2 models.
# The R-squared for the list-wise deleted dataset is ~0.11 vs. the imputed dataset's ~0.09.