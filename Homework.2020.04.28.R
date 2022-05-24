
library(dplyr)
library(quantmod)
#####Problem 1#####
#1.1

mySMA <- function (price,n){
  sma <- c()
  sma[1:(n-1)] <- NA
  for (i in n:length(price)){
    sma[i]<-mean(price[(i-n+1):i])
  }
  sma <- reclass(sma,price)
  return(sma)
}

#Exemple
price<-c(1,1.2,1.4,1.5,2,3,2,4)
mySMA(price,5)

# 1.2.

correlation<-function(vector1,vector2){
  corr<-c()
  corr<-cov(vector1,vector2)/(sd(vector1)*sd(vector2))
  print(corr)
}

v1<-c(1,2,4,5,2,2,4,2)
v2<-c(2,5,5,3,13,4,5,3)
cor(v1,v2)
correlation(v1,v2)

#####Problem 2#####

x = seq(1, n)
prime_numbers=c()
for (i in 2:100) {
  if (any(x == i)) {
    prime_numbers = c(prime_numbers, i)
    x = c(x[(x %% i) != 0], i)
  }
}

#####Problem 3#####

library(tidyquant)
library(dplyr)
library(tidyverse)
library(lubridate)


few<-tq_get(c("FB"))%>%
  select(date, adjusted)

data<-few%>%
  mutate(period1=EMA(adjusted,26),period2=EMA(adjusted,12),MACD=period2-period1)%>%
  mutate(signal.line=EMA(MACD,9))%>%
  mutate(buy.sell=ifelse(MACD<0 & lag(MACD)>0 ,"Sell",
                         ifelse(MACD>0 & lag(MACD)<0 ,"Buy","")))

data2<-data%>%
  mutate(buy.sell=ifelse(MACD<signal.line & lag(MACD)>lag(signal.line) ,"Sell",
                         ifelse(MACD>signal.line & lag(MACD)<lag(signal.line),"Buy","")))%>%
  mutate(profitcoeff = (adjusted/lag(adjusted)),
         profitcoeff = ifelse(is.na(profitcoeff),1, profitcoeff),
         benchmark = cumprod (profitcoeff),
         base.line.strategy = 100*benchmark,
         stock=ifelse(MACD<signal.line,"no stock", "stock"),
         trading.strategy = ifelse(stock == "no stock", lag(trading.strategy), lag(trading.strategy*profitcoeff)))

     
