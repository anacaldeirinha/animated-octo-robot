#####Problem 1##### 

library(tidyquant)
library(tidyverse)
library(RcppRoll)
library(data.frame)



FromDate<-"2012-01-03"
ToDate<-"2021-10-26"

info<-tidyquant::tq_get("GOOG", from = lubridate::ymd(FromDate),
                        to=lubridate::ymd(ToDate))%>%
  dplyr::select(symbol, date, adjusted)



dates<-base::data.frame(Dates=base::rep(base::seq.Date(from = lubridate::ymd("2012-01-03"),
                                                       to=lubridate::ymd("2021-10-26"),
                                                       by="day")),
                        Symbol = c(base::rep("GOOG",3585)))
                      
                        
DATA<-dates%>%
  dplyr::left_join(info, by=c("Dates"="date", "Symbol" = "symbol"))%>%
  dplyr::group_by(Symbol)%>%
  tidyr::fill(adjusted, .direction = "downup")

SMA<-DATA%>%
  dplyr::mutate(sma20 = TTR::SMA(adjusted, n= 20),
                Stand.Dev. = RcppRoll::roll_sdr(adjusted, n= 20),
                Upper.Bound = sma20 + 2 * Stand.Dev.,
                Lower.Bound = sma20 - 2* Stand.Dev.,
                Strategy = ifelse(adjusted > Upper.Bound, "Sell", ifelse(adjusted < Lower.Bound, "Buy", "Hold")))
                      
#####Problem 2#####
info2<-tidyquant::tq_get("GOOG", from = lubridate::ymd(FromDate),
                                                to=lubridate::ymd(ToDate))%>%
                          dplyr::select(symbol, date, adjusted)
                      
 
RSIdata<-info2%>%
  mutate(Gains=ifelse(adjusted > lag(adjusted),adjusted - lag(adjusted), "0"),
         Losses = ifelse(adjusted < lag(adjusted),adjusted - lag(adjusted), "0"))%>%
  slice(2:2470)


RSIdata$Gains<-as.numeric(as.character(RSIdata$Gains))
RSIdata$Losses<-as.numeric(as.character(RSIdata$Losses))

RSIdata2<-RSIdata%>%
  mutate(Gains.RSI = RcppRoll::roll_sumr(Gains, n= 14)/14,
         Losses.RSI = RcppRoll::roll_sumr(Losses, n= 14)/14,
         RSI = 100 - (100/(1 + (Gains.RSI/Losses.RSI))),
         Strategy = ifelse(RSI > 65, "Sell",ifelse(RSI < 35, "Buy", "Hold")))


