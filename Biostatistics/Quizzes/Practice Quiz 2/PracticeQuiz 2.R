library(dplyr)
library(MASS)
library(AER)
library(ROSE)
library(pROC)
library(mlbench)
library(foreign)

# Problem 1 ####

d = get(data(BostonHousing))

dim(d)
names(d)


threshold = median(medv)
d$medv = ifelse(d$medv >= threshold, 1, 0)
head(d)
attach(d)

base = glm(medv~., data = d, family = "binomial")
final1 = stepAIC(base)
summary(final1)
paste0(round(exp(summary(final1)$coef[,1]),3))
paste0(round(((exp(summary(final1)$coef[,1]))-1)*100,3),"%")
round(exp(summary(final1)$coef[,1]),3)

pp1 = predict(final1, type = "response")

roc = roc.curve(medv, pp1)
names(roc)
roc$thresholds
best_threshold_index = which.max(roc$true.positive.rate - roc$false.positive.rate)
optimal.threshold = roc$thresholds[best_threshold_index]

# Problem 2 ####

d2 = read.dta("seizure.dta")
d2 <- d2[-c(1)]
d2$trt <- factor(d2$trt)
head(d2)

base2 <- glm(y~.,data=d2,family=poisson)
final2 = stepAIC(base2,trace=0)
summary(final2)
1-pchisq(final2$deviance,final2$df.residual) #goodness of fit test
#p = 0, so bad model

#interpreting final2 model coefficients:
#note: no vars were considered significant
paste0(round(exp(summary(final2)$coef[,1]),3))
#For trt: "the expected number of seizures is 0.835 times lower for people who are trt"
#For age: "the expected number of seizures is 0.985 times lower for people who age by a year"
paste0(round(((exp(summary(final2)$coef[,1]))-1)*100,3),"%") 
#"For age: if someone is a trt, the expected number of seizures is decreased by 16.5%"
#For trt: "if someone is a year older, the expected number of seizures is decreased by 1.5%"

#running a dispersion test:
dispersiontest(final2)
#dispersion is >1 by a lot. Need to use quasipoisson

final3 <- glm(final2$call,family=quasipoisson,data=d2)
summary(final3)
#model coefficient information and interpretation is same. However, p-vals are different.
#no vars were found to be significant this time either, though.
