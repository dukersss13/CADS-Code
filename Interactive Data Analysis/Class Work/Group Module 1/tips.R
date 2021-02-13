setwd("C:/Users/Duker/Desktop/Fall 2020/CS 614/Class Work/Group Module 1")
library(dplyr)
library(plyr)
library(ggplot2)

tips = read.csv("tips.csv")
attach(tips)
names(tips)

# Tip rate vs. Sex
tip.rate = TIP/TOTBILL

qplot(TOTBILL, tip.rate, xlab = "Total Bill", ylab = "Tip Rate", col = SEX)

model = lm(tip.rate~SEX+SMOKER, data = tips)
anova(model)

boxplot(tip.rate~SEX, xlab = "Sex", ylab = "Tip Rate",
        names = c("Male", "Female"))

# Tip rate vs Combinations of Smoker Status + Sex
smoker.sex = paste(SMOKER, SEX)
boxplot(tip.rate~smoker.sex, xlab = "Combinations of Smoker + Sex",
        ylab = "Tip Rate",
        names = c("Non-smoker Males", "Non-smoker Females",
                  "Smoker Males", "Smoker Females"))

# ANOVA
anova(lm(tip.rate~SMOKER, data = tips))
anova(lm(tip.rate~SEX, data = tips))

# Question 2 ####
# At what time & day is the tip rate the highest/lowest? 

boxplot(tip.rate~DAY, xlab = "Day", ylab = "Tip Rate",
        names = c("Thursday", "Friday", "Saturday", "Sunday"))
boxplot(tip.rate~TIME, xlab = "Time", ylab = "Tip Rate",
        names = c("Morning", "Night"))

time.day = paste(TIME, DAY)
time.day2 = mapvalues(time.day, c("0 3", "0 4", "1 3", "1 4", "1 5", "1 6"), 
          c("Morning Thursday", "Morning Friday", "Night Thursday",
            "Night Friday ", "Night Saturday ", "Night Sunday "))

boxplot(tip.rate~time.day2, xlab = "Time & Day", ylab = "Tip Rate")

model = lm(tip.rate~TIME+DAY, data = tips)
summary(model)
anova(model)

plot(tip.rate ~ tips$SIZE, data = tips, xlab = "Party Size", ylab ="Tip Rate")

boxplot(tip.rate ~ tips$SIZE, data = tips,xlab = "Party Size", ylab ="Tip Rate")

anova(lm(tip.rate~SIZE, data = tips))
