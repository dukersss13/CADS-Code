library(faraway)
library(ROSE)
library(AER)
library(plyr)
library(MASS)
library(dplyr)
library(foreign)

data(wcgs)

# Problem 1 ####
# a ####
str(wcgs)

new.wcgs = na.omit(wcgs)
attach(new.wcgs)
new.wcgs$arcus = as.factor(ifelse(new.wcgs$arcus == "absent", 0, 1))
new.wcgs$typechd = as.factor(ifelse(new.wcgs$typechd == "none", 0, 1))
new.wcgs$chd = as.factor(ifelse(new.wcgs$chd == "no", 0, 1))
new.wcgs$dibep = as.factor(ifelse(new.wcgs$dibep == "A", 0, 1))

values = levels(behave)
new.wcgs$behave = mapvalues(factor(new.wcgs$behave), from = values, to = seq(length(values)))

new.wcgs = new.wcgs %>% select(-c(timechd, typechd, behave))

# b ####
base = glm(dibep~., data = new.wcgs, family = "binomial")
final1 = stepAIC(base)

summary(final1)

# The model was able to predict dibep best by using age + height + sdp + chol + cigs and chd.
# Quick overview of the coefficients: All of the features chosen are statistiscally significant
# with the exception of chol.

# The coefficients mean for example: a unit increase in age (a continuous var) will increase the
# log(odds of being passive) by 0.027.
# Interpreation for a binary feature: If the person has coronary heart disease (1) then his
# log(odds of being passive) will increase by 0.00126.


# c ####
pp1 = predict(final1, type = "response")
roc1 = roc.curve(dibep, pp1)
roc1

# The AUC obtained under the ROC is 0.596. This is obviously not an ideal AUC score we're striving
# for. However, given the limited time of the exam, I cannot afford to further investigate to why
# the model yielded an undesireable AUC score.

# d ####
best_threshold_index = which.max(roc1$true.positive.rate - roc1$false.positive.rate)
roc1$thresholds[best_threshold_index]

# e ####
final2 = stepAIC(base, ~.^2, trace = 0)
final3 = stepAIC(base, ~.^3, trace = 0)

pp2 = predict(final2, type = "response")
roc2 = roc.curve(dibep, pp2)
roc2

pp3 = predict(final3, type = "response")
roc3 = roc.curve(dibep, pp3)
roc3

# The 2 new models yield the same results in terms of the AUC score.
# They both performed better than the original log model that did not have interactions.

# f ####

# The first logistic model performed poorly, only having an AUC of 0.596. The following 2 interactive
# models performed slightly better. A guess on this improvement could be because some features
# are dependent on one another, thus 2 & 3 way interactions between them will yield better results.
# If given more time, an approach I could do to improve the accuracy + AUC of all these models
# could be to look more into the variables' meanings & do a better job of feature selecting.

# Problem 2 ####

# a ####

d = read.table("https://data.princeton.edu/wws509/datasets/ships.dat")
names(d)
str(d)

# d$construction = mapvalues(factor(d$construction), from = levels(d$construction), 
#                             to = seq(length(levels(d$construction))))
# 
# d$operation = mapvalues(factor(d$operation), from = levels(d$operation), 
#                             to = seq(length(levels(d$operation))))

poisson.base = glm(damage~., offset(log(months)), data = d, family = poisson) 
poisson.final = stepAIC(poisson.base, trace = 0)
summary(poisson.final)

# b ####
1-pchisq(poisson.final$deviance, poisson.final$df.residual)

# p = 0 so bad model

# c ####
dispersiontest(poisson.final)

# The dispersion is 1.956 which is over 1. 

# d ####
quasi.poisson =  glm(poisson.final$call, family = quasipoisson, data = d)
summary(quasi.poisson)
1 - pchisq(quasi.poisson$deviance, quasi.poisson$df.residual)

# After refitting the model with Quasi-Poisson, I can observe that there isn't any major changes
# when it comes to the coefficients. However the number of significant features has been massively
# reduced. With the Quasi-Poisson model, only 4 of the features are ruled statistically significant.
# Using Chi-Sq GoF test, the Quasi-Poisson model provided us better results.