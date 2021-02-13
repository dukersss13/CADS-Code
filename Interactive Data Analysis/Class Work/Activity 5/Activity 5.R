library(dplyr)
library(ggplot2)
library(mosaicData)

data("SnowGR")
data("mtcars")

# Problem 1 ####
hist.1 = ggplot(data = mtcars, mapping = aes(x = mpg, y = ..density..)) +
  geom_histogram(color = "black", fill = "yellow", alpha = 0.3)
hist.1

# Problem 2 ####
dens.2 = hist.1 + geom_density(color = "cyan", fill = "cyan", alpha = 0.2) +
  geom_hline(yintercept = 0.07)

dens.2

# Problem 3 ####
mtcars$am = as.factor(mtcars$am)
mpgAuto = ggplot(mtcars, aes(x = mpg, y = ..density..)) + 
  geom_density(aes(group = am, color = am))
mpgAuto

# Problem 4 ####

# Change the color to green if you want the points to be green
plot.4a = ggplot(data = mtcars, aes(x = mpg, y = qsec))+
  geom_point(color = "blue")
plot.4a

plot.4b = ggplot(data = mtcars, aes(x = mpg, y = qsec))+
  geom_point(aes(color = drat))
plot.4b

plot.4c = ggplot(data = mtcars, aes(x = mpg, y = qsec))+
  geom_point(aes(color = am))
plot.4c

plot.4d = ggplot(data = mtcars, aes(x = mpg, y = qsec))+
  geom_point(aes(color = factor(am)))
plot.4d

# Problem 5 ####

plot.5 = ggplot(data = mtcars, mapping = aes(x = mpg, y = qsec, group = factor(am))) +
  geom_point(size = 2, aes(shape = factor(am), color = factor(am))) +
  scale_color_manual(name = "Transmission Type", values = c("magenta", "blue")) +
  scale_shape_manual(name = "Transmission Type", values = c("circle", "triangle")) +
  theme(legend.direction = "horizontal") +
  labs(x = "MPG", y = "Quarter Mile Time (sec)", size = 12)

plot.5
