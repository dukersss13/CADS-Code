#1)
x = 5 + 8i
y = -6 + 7i
u = x + y
v = x*y
w = x/y 
u = exp(x)
r = sqrt(y)
s = x*y^2

v
w
u
r
s

#2)
(3+6i)*(-7-9i)
(5+4i)/(5-4i)
3/(2i)

#3)
exp(-2.1^3) + 3.47*log(14) + sqrt(sqrt(287))
(3.4)^7 *log(14) + sqrt(sqrt(287))
(cos(4.12*pi/6))^2
cos((4.12*pi/6)^2)

#4) 
X = 6
(X<10) 
(X == 10)
(X >= 4)
(x != 7)

#5) 
6>3+8 
6+3>8
4>(2+9) 
(4<7)+3 
4<7+3 
(4<7)*5
4<(7*5) 
2/5>=5

#6) 
A= matrix(c(3,7,-4,12,-5,9,10,2,6,13,8,11,15,5,4,1), byrow = T, nrow=4)
V = A[1:4,2]
W = A[2,1:4]

#7)
random = rnorm(20,10,5)
random
trim = function(x) sort(x)[-c(1,2,length(x) - 1,length(x))]
trim(random)

#8)
data = read.table("temperature.csv", sep = ',',header = T)
attach(data)
names(data)
str(data)
mean(temperature)
mean(rain)
max(rain)
max(temperature)
min(rain)
min(temperature)
sd(temperature)
sd(rain)
var(rain)
var(temperature)
range(rain)
range(temperature)
tapply(temperature,month,mean)
max(tapply(temperature,month,mean)) #August is the hottest month
min(tapply(temperature,month,mean)) #Jan is the coldest month

#10)
Random2 = rnorm(100,50,50)
hist(Random2)
which(abs(Random2 - 100) == min(abs(Random2 - 100)))
#81 is closest to 100
