setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tibble)
library(dplyr)

load("map.coords.RData")
ccA = read.csv("ccA.csv")
ccB = read.csv("ccB.csv")
gdp = read.csv("gdp.csv")

# Task 5
df = merge(ccA, ccB, all = TRUE)
df2 = cbind(df, gdp)

# Problem 1 ####
left = merge(df2, coords, all.x = TRUE)
right = merge(df2, coords, all.y = TRUE)
inner = merge(df2, coords, all = FALSE)

dim(left)
dim(right)
dim(inner)

# Problem 2 ####

# 2a)
data = inner %>%
  select(gdp, lat)

quantiles = quantile(data$gdp)
quantiles

# 2b)
gdp.q = findInterval(data$gdp, quantiles, rightmost.closed = TRUE)
table(gdp.q)

# 2c) 
rev_gdp.q = factor(gdp.q,levels = 4:1)
table(rev_gdp.q)

# 2d)
q1 = data %>%
  filter(gdp <= quantiles[2])
q1.lat = mean(abs(q1$lat))
q1.lat

q2 = data %>%
  filter(gdp >= quantiles[2] & gdp <= quantiles[3])
q2.lat = mean(abs(q2$lat))
q2.lat

q3 = data %>%
  filter(gdp >= quantiles[3] & gdp <= quantiles[4])
q3.lat = mean(abs(q3$lat))
q3.lat

q4 = data %>%
  filter(gdp >= quantiles[4] & gdp <= quantiles[5])
q4.lat = mean(abs(q4$lat))
q4.lat
