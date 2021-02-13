setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tibble)
library(dplyr)

ccA = read.csv("ccA.csv")
ccB = read.csv("ccB.csv")
gdp = read.csv("gdp.csv")

str(ccA)
str(ccB)
str(gdp)

#5)
df = merge(ccA, ccB, all = TRUE)
str(df)

#6)
df2 = cbind(df, gdp)
str(df2)

# country variable type is factor???

#7)
filter_df2 = df2 %>% 
    filter(year > 1980, gdp > 20000)

#8)
df3 = data.frame()

years = unique(df2$year)

for (i in years){
  yearlyTable = df2 %>%
    filter(year == i)
  
  medianGDP = median(yearlyTable$gdp)
  delta_list = c()
  
  for (gdp in yearlyTable$gdp){
    
    delta = abs(gdp - medianGDP)
    delta_list = append(delta_list, delta)
  }
  index = which(delta_list == min(delta_list))
  df3 = rbind(df3, yearlyTable[index,])
}

unique(df3)

#9)
continents = unique(df2$continent)
df4 = data.frame()

for (ii in continents){
  yearlyTable = df2 %>%
    filter(continent == ii)
  
  medianGDP = median(yearlyTable$gdp)
  delta_list = c()
  
  for (gdp in yearlyTable$gdp){
    
    delta = abs(gdp - medianGDP)
    delta_list = append(delta_list, delta)
  }
  index = which(delta_list == min(delta_list))
  df4 = rbind(df4, yearlyTable[index,])
}

unique(df4)
