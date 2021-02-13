setwd("C:/Users/Duker/Desktop/Fall 2020/CS 614/Class Work/Group Module 3")
library(lme4)
library(lmerTest)
library(MASS)

data = read.csv("politeness_data.csv")

# Random intercept model with subject as the random effect and gender + attitude as predictor variables. 
model1 = lmer(frequency~1 + (1|gender) + (1|attitude), data = data)
summary(model1)

# Random slope model with subject as the random effect, but with attitude as the slope effect. Still
# have gender + attitude as predictor variables. 

model2 = lmerTest::lmer(frequency~ subject + (subject|attitude), data = data,
                        na.action = na.exclude)
summary(model2)

# Random intercept model with subject and scenario as the random effects and gender + attitude as
# predictor variables. Still have gender + attitude as predictor variables.
model3 = lmer(frequency~gender+attitude + (1|scenario) + (1|subject), data = data)
summary(model3)

# Problem 1 ####
var.ICC = function(sigmaB, sigmaW){
  return(round(sigmaB/(sigmaB + sigmaW),3))
}

gender.sigmaB = 5834.9
sigmaW = 1272.8
model1.gender.ICC = var.ICC(gender.sigmaB, sigmaW)
model1.gender.ICC
# Looking at the ICC for gender, we can see that ~82% of the variation is associated within the 
# gender class.

attitude.sigmaB = 160.4
model1.attitude.ICC = var.ICC(attitude.sigmaB, sigmaW)
model1.attitude.ICC
# Similarly, the ICC for attitude is only 11%, which implies low variation within that class.

summary(model2)
# There is statistical significance when looking at the fixed effects of subjectM3, M7 & the Intercept.

summary(model3)

# Problem 3 ####
m1 = lmer(frequency~1 + (1|gender) + (1|attitude), data = data, REML = F)
m2 = lmerTest::lmer(frequency~ subject + (subject|attitude), data = data,
                    na.action = na.exclude, REML = F)
m3 = lmer(frequency~gender+attitude + (1|scenario) + (1|subject), data = data, REML = F)

anova(m1, m2, m3)
AIC(m1, m2, m3)

# Model 3 has the AIC & BIC thus it is the simplest model with the best performance.

# Problem 4 ####
r1 = unlist(ranef(model1))
plot(density(r1))

r2 = unlist(ranef(model2))
plot(density(r2))

r3 = unlist(ranef(model3))
plot(density(r3))

# The plots of the random effects all resemble a bell-shaped curve thus confirming these effects
# are normally distributed.