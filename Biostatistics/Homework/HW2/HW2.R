library(foreign)
library(ROSE)
library(ResourceSelection)
library(MASS)
library(dplyr)

setwd("C:/Users/Duker/Desktop/Fall 2020/CS 624/Homework/HW2")
data = read.dta("wcgs.dta")

data$chd69 <- ifelse(data$chd69=="Yes", 1, 0)

x = data %>%
  select(-c(typchd69, id, t1, agec, wghtcat)) 

x = na.omit(x)

attach(x)
summary(x)
str(x)

base = glm(chd69~., data = x, family = "binomial")
final1 = stepAIC(base, trace = 0)
final2 = stepAIC(base, ~.^2, trace = 0)

pp1 = predict(final1, type = "response")
pp2 = predict(final2, type = "response")

roc.curve(chd69, pp1)
roc.curve(chd69, pp2)

hoslem.test(chd69, fitted(final1))
hoslem.test(chd69, fitted(final2))

theta = seq(0,1,0.01)
sens1 = rep(0,length(theta))
spec1 = rep(0,length(theta))
sens2 = rep(0,length(theta))
spec2 = rep(0,length(theta))
dist1 = rep(0,length(theta))
dist2 = rep(0,length(theta))

#want to replace values in pps w/ 0 or 1 depending on > threshold

for (i in 1:length(theta)){
  low_thresh1 <- as.numeric(pp1>theta[i])
  low_thresh2 <- as.numeric(pp2 >theta[i])
  sens1[i]=sum(chd69*low_thresh1)/sum(chd69)
  sens2[i]=sum(chd69*low_thresh2)/sum(chd69)
  spec1[i]=sum((1-chd69)*(1-low_thresh1))/(length(chd69)-sum(chd69))
  spec2[i]=sum((1-chd69)*(1-low_thresh2))/(length(chd69)-sum(chd69))
  dist1[i]=(1-spec1[i])^2+(1-sens1[i])^2
  dist2[i]=(1-spec2[i])^2+(1-sens2[i])^2
}
which.min(dist1)
which.min(dist2)
theta[which.min(dist1)]
theta[which.min(dist2)] 


plot(1 - spec1, sens1, type="l", xlab = "False Positive Rate", ylab = "True Positive Rate")  
points(1 - spec1[which.min(dist1)], sens1[which.min(dist1)], pch = 17, col = "red")

plot(1 - spec2,sens2, type = "l", xlab = "False Positive Rate", ylab = "True Positive Rate") 
points(1 - spec2[which.min(dist2)], sens1[which.min(dist1)], pch = 17, col = "red")


