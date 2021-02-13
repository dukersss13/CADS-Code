library(carData)
library(car)
library(MASS)
setwd("C:/Users/Duker/Desktop/Fall 2020/CS 624/Quizzes/Practice Quiz 1")

# Problem 1 ####
# 1a ####
data = read.table("wcgs_short.txt", header = T)
head(data)

# 1b ####
dim(data)
names(data)
attach(data)

summary(data)
hist(age, freq = F)
hist(bmi, freq = F)
hist(smoking, freq = F)
hist(chd, freq = F)
boxplot(data)

# 1c ####
scatterplotMatrix(~age + bmi + smoking + chd)

# 1d ####
mlm = lm(bmi ~., data = data)
stepAIC(mlm)

# 1e ####
best.lm = lm(bmi~ smoking + chd)
plot(best.lm, which = c(1))
plot(best.lm, which = c(2))
plot(best.lm, which = c(3))
plot(best.lm, which = c(4))
plot(best.lm, which = c(5)) #Cook's Distance (Residuals vs. Leverage)
summary(best.lm)

#1g ####
smoking.example = 0
chd.example = 1
newdata = data.frame(smoking = smoking.example, chd = chd.example)

predict(best.lm, newdata, interval = 'prediction')
