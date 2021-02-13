#1) 

p = c() #Creating empty vectors
diff = c()
sum = c()
for (n in 0:200) {
  k = ((-1)^n)*(1/(2*n+1)) #Leibniz's series formula
  p = append(p, k, after= length(p)) #Append value of k into the empty vector p
  s = sum(p) #Sum up all values of p calculated up to x in [0,200]
  d = (pi/4) - s #Difference between actual pi/4 and the approximation up to x in [0,200]
  diff = append(diff,d,after=length(diff))
  sum = append(sum,s,after=length(sum)) #Appending the sum and difference 
}

n = c(0:200)
par(mfrow=c(1,2))
plot(n,diff,type="l", xlab = "n", ylab="Pi/4 - Sum(n)")
plot(n,sum,type = "l",xlab = "n", ylab="Sum(n)")


#2)
z = seq(800,1500,1)
pd = dnorm(z,1152,84)

par(mfrow=c(1,3))
plot(z,pd,type= "l")
polygon(c(z[z>=1250],1250), c(pd[z >= 1250], 0), col = "red")

plot(z,pd,type= "l")
polygon(c(z[z<=1200],1200), c(pd[z <= 1200],0), col = "red")

plot(z,pd,type= "l")
polygon(c(1000,z[z>=1000 & z<= 1100],1100),c(0,pd[z>= 1000 & z<=1100],0),col = "red")

#3) 
temp = read.table("temperature.csv", sep = ",", header = T)
temp = temp[,3:7]
attach(temp)
names(temp)
par(mfrow=c(1,1))
m = factor(month) #Assigning the 'month' to be factor
plot(m, temperature, xlab = "Month", ylab = "Daily Maximum Temperature")

#4)
prob = seq(0.1,0.9,0.1) #Probability vector from 0.1 to 0.9
success = seq(0,8,1) #Number of possible successes in 8 trials
p = c()
for (j in prob){
for (i in success) {
  c = factorial(8)/(factorial(8-i)*factorial(i))
  prob = c*(j)^i*(1-j)^(8-i)
  p = append(p,prob,after=length(p))
  }
}
#Dividing success according to the probability into 9 vectors.
#Each vector holds the probability of x number of success given the prob of success
suc0 = p[1:9]
suc1 = p[10:18]
suc2 = p[19:27]
suc3 = p[28:36]
suc4 = p[37:45]
suc5 = p[46:54]
suc6 = p[55:63]
suc7 = p[64:72]
suc8 = p[73:81]

#Graphing the 9 plots of probability ranging from 0.1 to 0.9
par(mfrow = c(3,3))
barplot(suc0,xlab = '# of successes',ylab = 'Probability',main = "p = 0.1")
barplot(suc1,xlab = '# of successes',ylab = 'Probability',main = "p = 0.2")
barplot(suc2,xlab = '# of successes',ylab = 'Probability',main = "p = 0.3")
barplot(suc3,xlab = '# of successes',ylab = 'Probability',main = "p = 0.4")
barplot(suc4,xlab = '# of successes',ylab = 'Probability',main = "p = 0.5")
barplot(suc5,xlab = '# of successes',ylab = 'Probability',main = "p = 0.6")
barplot(suc6,xlab = '# of successes',ylab = 'Probability',main = "p = 0.7")
barplot(suc7,xlab = '# of successes',ylab = 'Probability',main = "p = 0.8")
barplot(suc8,xlab = '# of successes',ylab = 'Probability',main = "p = 0.9")