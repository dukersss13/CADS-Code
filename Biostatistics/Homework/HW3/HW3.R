library(dplyr)
library(MASS)
library(VGAM)
library(mlbench)

rm(list = ls())
d = get(data(BostonHousing))
str(d)
q = quantile(d$medv, c(0.25, 0.75))

d$medv = ifelse(d$medv < q[1], 0, ifelse(d$medv >= q[1] & d$medv <= q[2], 1, 2))
table(d$medv)

model1 = vglm(medv~., multinomial(refLevel = 1), data = d)
summary(model1)

# Looking at the summary of the coefficients, we can eliminate variables that aren't statistically
# significant to simplify the model such as: crim, zn, indus, cha1. Since my reference level = 1,
# these variables don't contribute much to the log odds of being between the 25th & 75th percentile
# Similarly, they aren't significant when it comes to predicting the log odds of being above the
# 75th percentile.

# Problem 2 #### 
model2 = vglm(medv~., family = propodds(), data = d)
lrtest(model1, model2)


