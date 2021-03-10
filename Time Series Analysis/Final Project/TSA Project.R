library(TSA)
library(forecast)
library(tidyverse)
library(sarima)

arimaFunction = function(data, n){
model = auto.arima(data)
model
#auto.arima confirms that the data set needs to be differenced once.
#It provides the best ARIMA model with AR(2), MA(2) & d = 1.

print(checkresiduals(model))
#The residuals show the data set with difference = 1.

futurePrice = forecast(model, h = n)

return(futurePrice)
}

Holt = function(data,n){
  par(mfrow = c(1,1))
  
  HW = HoltWinters(data, gamma = FALSE)
  prediction = forecast(HW, h = n)
  plot(HW)
  print(HW$alpha)
  print(HW$beta)
  return(prediction)
}

# I want to analyze Square, Tesla, Draftkings, Pinterest & Opendoor
library(quantmod)

stocks_list = c("SQ", "TSLA", "DKNG", "PINS", "OPEN")

for (i in stocks_list){
  getSymbols(i,from = "2020-01-01", to = Sys.Date())
}

stocks = list(SQ, TSLA, DKNG, PINS, OPEN)

closing = lapply(stocks, function(s) {
  return(s[, 4])
})

SQ = closing[[1]]

acf(SQ)
pacf(SQ)
SQ30 = arimaFunction(SQ, 20)
SQ30

HW.SQ30 = Holt(SQ, 20)
HW.SQ30

