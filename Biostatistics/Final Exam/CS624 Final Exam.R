setwd("C:/Users/Duker/Desktop/Fall 2020/CS 624/Final Exam")
rm(list=ls())
library(MASS)
library(ROSE)
library(dplyr)
library(plyr)
library(regclass)
library(rattle.data)
library(VGAM)
library(survival)
library(survminer)
defaultW <- getOption("warn") 
options(warn = -1)

# Problem 1 ####
pension = read.csv("pension.csv")
pension = pension %>% select(-c(X, id)) %>%
  na.omit(pension)

base.model1 = lm(wealth89~., data = pension)
summary(base.model1)
final1 = stepAIC(base.model1, trace = 0)
summary(final1)
# Looking at the summary of our model, a person is estimated to have negative wealth (-$593), given
# no effects from all the other predictors. There seems to be a huge emphasis on how retirement
# contribution and investing habits (stocks, IRA) affect one's overall wealth.
# The majority of variables related to financial contribution holds significance for the linear regression model.
# The best/simplest model from stepAIC holds an R-squared of 0.2938, indicating a poor fit.
# This could simply be maybe these predictors just aren't ideal to be used for predicting wealth.


# Problem 2 ####
travel = read.table("Travel.txt", header = T)
travel = na.omit(travel)

summary(travel)
str(travel)

travel$orig_destination_distance = as.numeric(travel$orig_destination_distance)
travel = na.omit(travel)
for (i in 2:length(travel)){
  travel[,i] = as.factor(travel[,i])
}
travel$srch_adults_cnt = as.numeric(travel$srch_adults_cnt)
travel$srch_rm_cnt = as.numeric(travel$srch_rm_cnt)
travel$orig_destination_distance = as.numeric(travel$orig_destination_distance)
str(travel)

base.model2 = glm(is_booking~orig_destination_distance+
                   is_mobile+is_package+channel+
                   prop_is_branded+srch_adults_cnt + srch_rm_cnt+
                   prop_starrating+distance_band+
                   hist_price_band+popularity_band, data = travel, family = "binomial")
final2 = stepAIC(base.model2, trace = 0)
summary(final2)
exp(final2$coefficients)-1

p2 = predict(final2, type = "response")
roc.curve(travel$is_booking, p2)

# Within this abundant list of variables, it seems like only a subset of them has significance
# when it comes to predicting whether a customer books a hotel room.
# For ex: whether a customer is using a mobile app (is_mobile) has statistical significance.
# Whether or not the room comes in a package (is_package) is also an important predictor.
# Another important predictor worth noting is the popularity band of hotels relative to each
# other in the same destination.

# Looking at the coefficients, we can exponentiate them to make it easier to interpret the
# odds of booking a hotel room. For ex: if the user is using a mobile app, then the odds
# of booking will decrease by ~0.115. If the booking is included in a package, the odds
# of book decrease by ~0.6.

# Problem 3 ####
fitglm = read.delim("FITglm2.txt", sep="\t")

fitglm2 = fitglm %>%
  select(-c(alloc, nosp)) %>% na.omit(fitglm)

summary(fitglm2)
risk.levels = levels(fitglm2$riskcat4)
fitglm2$riskcat4 = mapvalues(factor(fitglm2$riskcat4), from = risk.levels, to = seq(length(risk.levels)))
rt.levels = levels(fitglm2$rtgroup)
fitglm2$rtgroup = mapvalues(factor(fitglm2$rtgroup), from = rt.levels, to = seq(length(rt.levels)))
str(fitglm2)

base.model3 = glm(numnosp~., data = fitglm2, family = "poisson")
final3 = stepAIC(base.model3, trace = 0)
summary(final3)

1-pchisq(final3$deviance, final3$df.residual)
(exp(final3$coefficients) -1)*100

# The resulted poisson regression model from stepAIC only uses 2 covariates to predict the # of
# non-spinal bone fractures in women with low bone densities. "Trialyrs", the duration of follow-up
# is the only variable that holds statistical significance.
# Analyzing the effect sizes, for every unit increase in "trialyrs" or duration of follow-up,
# the # of expected fractures increases by 9.48%. Similarly, if the patient has spinal fractures,
# (frx = 1), the rate of fractures increases by 1.43%. The resulted chi-square test using the
# model's deviance & residual is 1, indicating the model was a good fit.

# Problem 4 ####
data(wine)
str(wine)

base.model4 = vglm(Type~., multinomial(refLevel = 1), data = wine)
summary(base.model4)
(exp(base.model4@coefficients)-1)*100

# Since the reference level chosen is 1, this means the 2 lines of coefficients produced will represent
# the log odds of type 2 and 3 in respect of type 1.
# Looking at Alcohol1, every unit increase will result in a -99% for the odds of one choosing type2 wine.
# Every increase of Alcohol2 will result in a -54% for the odds of one picking type3 wine.
# There are contrasting coefficients for both odds equations, such as every unit increase in Hue,
# will lead to a huge bump for the odds of Type2 but a decrease in the odds for Type3.

# Problem 5 ####
data(lung)
summary(lung)

df3 = na.omit(lung)
attach(df3)

surv.obj = Surv(time = time, event = status)

kmsurvival = survfit(surv.obj~1)
summary(kmsurvival)
plot(kmsurvival, xlab = "Time", ylab = "Survival Probability")

kmsurvival.sex = survfit(surv.obj~ sex)
summary(kmsurvival.sex)
plot(kmsurvival.sex, xlab = "Time", ylab = "Survival Probability")

# From the survival model plot in relation to Sex, we see that over time, males actually
# have a lower survival rate than women despite starting off with a higher survival probablity.

coxph.model = coxph(surv.obj~age+sex+ph.ecog+pat.karno+meal.cal+wt.loss, data = df3)
summary(coxph.model)
percentages = round(((exp(coxph.model$coefficients)-1)*100),3)
percentages
# Analyzing the hazard rates for each variable, we see that for every unit increase in age
# the subject is 0.56% more likely to die from lung cancer.
# Looking at ph.ecog, the more "bedbound" the subject is, the more likely he/she is to
# die from lung cancer. Their probability of dying increases by 55% for this predictor.