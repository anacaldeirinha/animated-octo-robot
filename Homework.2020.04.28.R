
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

i=100
stock=100





# 6. Simulate how the strategy preforms and compare it to a benchmark strategy
# of just buying and holding the stock.
# In order to do this start with a portfolio of 100$ invested in the stock on the first day
# and see how it performs. Example:
# I start with 100$ and a stock which costs 100$ at the beginning of my time period.
# I get a buy signal when the stock price is 90. I buy the stock.
# I get a sell signal to sell the stock when the price is 110. I sell it and 
# and don't get any more signals. I end up with 100 * 110 / 90 = 122.22 
# The benchmark portfolio is I buy the stock at 100 at the beginning and at
# the end of the period the stock price is 120. I end up with 120.
# 122.22 > 120. so the MACD strategy was beating the market.