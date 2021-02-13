#1)
blow = read.table("blowfly.txt", header = T)

attach(blow)

unique(flies)

length(unique(flies))

#353 numbers are unique since the first row isn't a number.

#2) 
p = 10000
t = 0
i = 0 
while(p < 1000000)
{
  p = p + p*0.06
  p = p + 10000
  t = t + 1
}
t

#3)
country = read.table("country.txt", header = T)
attach(country)
as.vector(Country[grep("^.{3}c",as.character(Country))])

#4)
area = function(r,V)
{ 
h = (3*V)/(r^2*pi)
A = pi*r*(sqrt(r^2 + h^2))
return(A)
}

V = 10
r = seq(0.1,10,0.1)
A = area(r,V)

plot(r,A,type = 'l')
which(A == min(A))
r[19]

#5)
fact = function(x)
{
  f = 1
  if (x<2) return (1)
  for (i in 2:x)
  {
    f = f*i
  }
  f
}
sapply(0:10,fact)

#6)
cellsT = read.table("cells.txt", header = T)
temp = read.table("temp.txt", header = T)

attach(cellsT)
attach(temp)

#a)
lapply(cellsT,typeof)
lapply(temp,typeof)

#b)
cell = read.table("cells.txt", header= T)
names(cell)[1] = "Cells"
names(cell)[2] = "Smoker"
names(cell)[3] = "Smoker's Age"
names(cell)[4] = "Smoker's Gender"
names(cell)[3] = "Smoker's Weight"

temperature = read.table("temp.txt", header = T)
names(temperature)[1] = "Temperature"
names(temperature)[3] = "City Population"
names(temperature)[6] = "Wet Days"

#c)
write.table(temperature)
write.table(cell)

