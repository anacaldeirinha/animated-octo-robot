#####Problem 1#####

# 5.Select the last observation for each stock, for each month. 
# In order to do this, first create a new column, which will show you the 
# year and the month. You can do this using the functions substr() or floor_date.

library(tidyquant)
library(dplyr)
library(tidyverse)


Dates<-data.frame(Dates=seq.Date(from = ymd("2019-01-01"),
                                 to = ymd("2021-04-01"),by = "day"))


stockpricesF<-tq_get("FB")

FinalF<-Dates%>%
  left_join(stockpricesF, by=c("Dates" = "date"))%>%
  mutate(rownumber=row_number(),
         symbol = "FB")%>%
  fill(adjusted)%>%
  select(1:2,adjusted)

stockpricesA<-tq_get("AMZ")

FinalA<-Dates%>%
  left_join(stockpricesA, by=c("Dates" = "date"))%>%
  mutate(rownumber=row_number(),
         symbol = "AMZ")%>%
  fill(adjusted)%>%
  select(1:2,adjusted)


stockpricesN<-tq_get("NFLX")

FinalN<-Dates%>%
  left_join(stockpricesN, by=c("Dates" = "date"))%>%
  mutate(rownumber=row_number(),
         symbol = "NFLX")%>%
  fill(adjusted)%>%
  select(1:2,adjusted)

  

Q2<-tq_get(c("FB","AMZN"))%>%
  filter(date>=ymd("2019-01-01"),
         date<=ymd("2019-07-01")|
           date>=ymd("2020-04-01"),
         date<=ymd("2020-07-01"))%>%
  arrange(symbol, desc(date))%>%
  filter(row_number() == 1 |
           row_number() == n()|
           row_number() == sum(symbol == "AMZN")|
           row_number() == (sum(symbol == "AMZN")+1))




#####Problem 2#####
#Use the dataframe from problem 1.2.
# Use the SMA function from the tidyquant package to calculate the 10day SMA 
# and the 26 day SMA for each of the 3 stocks. 
# How many times did the 10 day SMA line cross 26 day SMA line from below? 
# How many times did the 10 day SMA line cross 26 day SMA line from above?
# You can take a look at this article: https://www.investopedia.com/trading/macd/
# Essentially by cross from above/below I want you to find the buy/sell signals.