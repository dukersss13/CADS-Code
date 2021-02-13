library(foreign)
library(MASS)
library(AER)
#For this dataset, we are interested in running a Poisson regression 
#to model the count variable "shared_syr":

d=read.dta("C:\\Users\\riley\\OneDrive\\Desktop\\Riley Desktop Specific Stuff\\Adulting\\Chapman\\5th year\\Biostats\\Unit 2 Prep\\needle_sharing.dta")

#Cleaning the Model:
dim(d) #just to see what we're working with
summary(d) #just to see what we're working with

d=d[-which(d$sex=="Trans"),] #removing rows w trans bc so few samples
d=d[-which(d$hivstat==2),] #removing rows w hivstat==2 bc so few samples. Note, there aren't many w hivstat==1 either. 
#Idk why we didn't just make it a binary, whether they have HIV or not var. Whatever

d=within(d,{ 
  id=NULL #removing arbitrary ID var
  sex=factor(sex) #making categorical
  sexabuse=factor(sexabuse) #making categorical
  hivstat=factor(hivstat) #making categorical 
  polydrug=factor(polydrug) #making categorical
  homeless=factor(homeless) #making categorical
  
  #note: we have decided to measure "shared_syr" count. Therefore, we can remove all other redundant syr related content
  shsyryn=NULL #removing redundant variable
  sqrtnivd=NULL #removing redundant variable
  logshsyr=NULL #removing redundant variable
  sqrtninj=NULL #removing redundant variable
  shsyr=NULL #removing redundant variable
  ethn=factor(ifelse(d$ethn=="AA"|d$ethn=="White"|d$ethn=="Hispanic",d$ethn,"Other")) 
  #making ethnicity categorical, with 4 options
})

d=na.omit(d) #finally, removing rows w NAs
head(d)

#Running Poisson:
base <- glm(shared_syr~.,data=d,family=poisson) #uses all vars
final1=stepAIC(base)
summary(final1) #note: dispersion param = 1 info in summary specifies no overdispersion

#how, using chisq() by looking at residual deviance and DOF
1-pchisq(final1$deviance,final1$df.residual) #a "Goodness of Fit Test Using Residual Deviance", outputs a p-val
#this is 1-pchisq(residual deviance, degrees of freedom)
    #Ho: model is good
    #Ha: model is bad
    #result: overwhelmingly reject the null (bc p-val is ~0, so def smaller than 0.05). This is a really poorly fit model :/

#interpreting model coefficients:
paste0(round(exp(summary(final1)$coef[,1]),3)) #"the expected number (frequency) of y-var is ____ times higher (>1) or lower (<1) for people who are non-baseline x-var than baseline x-var."
paste0(round((exp(summary(final1)$coef[,1]))*100 - 1,3),"%") #"if someone is non-baseline x-var, the expected number (frequency) of y-var is increased (if -) or decreased (if +) by ____%"

#running a dispersion test now:
dispersiontest(final1)
#dispersion was found to be greater than one (12.34), therefore overdispersion (by a lot)
#will need to use quasipoisson
  
  #fitting quasipoisson
  final2 <- glm(final1$call,family=quasipoisson,data=d)
  summary(final2)
  #dispersion param = 14.05
  #the coefficients haven't changed, but the variance has when allowed for overdispersion.
  #therefore, the p-vals have changed. Homeless1 is only significant var.
  
  #fitting to negative binomial
  final3 <- glm.nb(final1$call,data=d)
  summary(final3)
  #notice that the coefficients HAVE changed this time
  #so the interpretation will be a diff