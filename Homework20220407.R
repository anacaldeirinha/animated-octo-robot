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
  mutate(year.date=substr(date,1,7))%>%
  group_by(symbol,year.date)%>%
  slice(n())

  
######Problem 2#####
SMA<-data%>%
  group_by(symbol)%>%
  mutate(ten.days=SMA(adjusted,10),twentysix.days=SMA(adjusted,26))

library(data.table)

below<-SMA%>%
  group_by(symbol)%>%
  mutate(cross=ifelse(ten.days>twentysix.days& shift(ten.days)<shift(twentysix.days) ,"Cross",""))

above<-SMA%>%
  group_by(symbol)%>%
  mutate(cross=ifelse(ten.days<twentysix.days& shift(ten.days)>shift(twentysix.days) ,"Cross",""))
