#####Problem 1#####


library(tidyquant)
library(dplyr)
library(tidyverse)
library(lubridate)

#1 and 2

#First tought
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


#Learn in class


data<- tq_get(c("AMZ","FB","NFLX"),
              from = "2019-01-01",
              to = "2021-04-01")%>%
  select (symbol, date, adjusted)



Dates <- data.frame(Date = rep(seq.Date(from = ymd("2019-01-01"),
                                      to = ymd ("2021-04-01"),
                                      by = "day"), 3),
                  Symbol = c(rep("AMZ",822), rep ("FB", 822), rep ("NFLX",822)))

Join <- Dates %>%
  dplyr:: left_join(data, by = c("Date" = "date", "Symbol"="symbol"))%>%
  group_by(Symbol)%>%
  fill(adjusted, .direction = "downup")
 

#3 and 4

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

#5
Q5<-tq_get(c("FB","AMZN"))%>%
  filter(date>=ymd("2019-01-01"),
         date<=ymd("2019-07-01")|
           date>=ymd("2020-04-01"),
         date<=ymd("2020-07-01"))%>%
  arrange(symbol)%>%
  mutate(m = floor_date(date ,unit = c("month")))%>%
  

######Problem 2#####
#Use the dataframe from problem 1.2.
# Use the SMA function from the tidyquant package to calculate the 10day SMA 
# and the 26 day SMA for each of the 3 stocks. 
# How many times did the 10 day SMA line cross 26 day SMA line from below? 
# How many times did the 10 day SMA line cross 26 day SMA line from above?
# You can take a look at this article: https://www.investopedia.com/trading/macd/
# Essentially by cross from above/below I want you to find the buy/sell signals.