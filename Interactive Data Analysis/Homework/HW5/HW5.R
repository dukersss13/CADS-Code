# HW4
library(ggplot2)
library(usethis)
library(devtools)
library(reshape2)
library(MarkovSCD)

BL = HM2$MassAve[HM2$Phase == "BL"]
TX = HM2$MassAve[HM2$Phase == "TX"]
sb = seq(30,90,10)

# Problem 1 ####
cv = dynamicsconv(tseries1 = BL, tseries2 = TX, nitvl = 10,statebounds = sb,lag = 6)
il1 = cv$ilength1[7]
il2 = cv$ilength2[7]
vv = validitycheck(tseries1 = BL, tseries2 = TX, ilength1 = il1,ilength2 = il2, statebounds = sb,lag = 6)
vv$norm

df.1 = data.frame("x" = sb,"y" = vv$norm)
df.1

ggplot(data = df.1, mapping = aes(x, y))+
  geom_point(size = 2, color= "black") +
  geom_line(size = 1, color = "black") +
  labs(y = "Norm", x = "Iteration Offset from A", title = "Home 209") +
  theme(plot.title = element_text(hjust = 0.5))

# Problem 2 ####
df.2 = data.frame("x" = seq(-4,2),
                   "+1" = vv$diagconfig[1,],
                   "0" = vv$diagconfig[2,],
                   "-1" = vv$diagconfig[3,],
                   check.names = FALSE)
df.2 = melt(df.2, id.vars =  "x")

ggplot(data = df.2, mapping = aes(x = x, y = value)) + 
  geom_line(aes(group = variable, color = variable)) +
  geom_point(size = 2, alpha = 0.6, aes(color = variable)) +
  labs(x = "Iteration Offset from A", y = "Mean Value") +
  scale_colour_manual(name = "Pos", 
                      values = c("green", "blue", "red")) +
  theme(legend.direction = "horizontal", legend.position = c(0.1,0.98),
        legend.key.size = unit(0.02, "cm"))

# Problem 3 ####
le = lageval(tseries = TX,statebounds = sb, lagrange = c(1,2,seq(3,60,3)))
state = le$diagbylag

df.3 = cbind(melt(state), "Lag" = le$lagrange)

ggplot(data = df.3, mapping = aes(x = Lag, y = value, group = L1))+
  geom_line(size = 1, alpha = 0.5, aes(color = L1)) +
  geom_point(size = 1.5, aes(color = L1, shape = L1)) +
  labs(y = "Probablity", color = "State", shape = "State") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.direction = "horizontal", legend.position = c(0.4,0.95),
        legend.key.size = unit(0.05, "cm"))
  

# Problem 4 ####
B = transmat(tseries = TX, statebounds = sb, lag = 6)
prob = B$prob

df.4 = melt(prob)

ggplot(data = df.4, mapping = aes(x = Var2, y = rev(Var1), label = round(value,2))) +
  geom_tile(color = "black", aes(fill = value)) +
  geom_text(size = 3) +
  labs(x = "DestBin", y = "SourceBin", title = "Home 209") +
  scale_fill_gradient(limits = c(0,1), low = "white", high = "orange", 
                      trans = "sqrt")


       