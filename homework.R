
nycflights13::flights

library(nycflights13)
library(tidyverse)



#PROBLEM 1

money<-100
while(money > 0) {

cointoss<-sample(c("win","loss"),1,prob=c(0.4,0.6))
if(cointoss=="win"){
  money<-money+bet
  bet<-1
}else if(bet>money){
    bet<-money
   } else {
   money<-money-bet
   bet<-bet*2
   }
print(money)
}

#HOMEWORK 17/03
#5.2.4.
#1. Find all flights that
#1.1
F.ArrivalDelay<-filter(flights, arr_delay >= 120)

#1.2
F.Houston<-filter(flights,(dest=="IAH"|dest=="HOU"))

#1.3
F.Operated<-filter()

#1.4
F.DepartedSummer<-filter(flights,month==7|month==8|month==9)

#1.5
F.Late<-filter(flights,arr_delay>120&dep_delay<=0)

#1.6
F.MadeUp<-filter(flights,dep_delay>=60&(sched_arr_time-arr_time>=30))

#1.7
F.DepartedBetween<-filter(flights,hour==5|hour==6)

#2.
#The function between can be used to check whether a numeric value falls in a
#specific range or not and to help facilitate some filtered researches.
F.DepartedBetween<-filter(flights,between(hour,5,6))
F.DepartedSummer<-filter(flights,between(month,7,9))

#3
F.missing<-!(filter(flights,dep_time(is.class(numeric))))

#4.

#5.3
#5.3.1
F.MissingValues<-arrange(flights, desc(is.na(dep_time)), dep_time)

#5.3.2.
F.Early<-arrange(flights,(dep_delay))

#5.3.3
F.Fastest<-arrange(flights,desc(distance/air_time))

#5.3.4
F.Farthest<-arrange(flights,desc(distance))
F.Clostest<-arrange(flights,distance)

#5.4.1
#5.4.1.1

select(flights,contains("time"))
select(flights,year,month,day)
select(flights,dep_time:arr_time)
select(flights,-(distance:minute))

#5.4.1.2
ns<-select(flights,day,day)
s<-select(flights,day)
#Nothing different between the two

#5.4.1.3

#With the any_of() function we can remove variables from a data frame, using the 
#- sinal before using the function.
#It´s also usefull having names of variables in a vector and then use the
#any_off() function.





#CLASS
f1<-flights%>%
  group_by(carrier)%>%
  summarise(MeanDeparture=mean(dep_delay,na.rm=T))%>%
  arrange(MeanDeparture)%>%
  filter(row_number()==1)

f2<-flights%>%
filter(month==1)%>%
 group_by(carrier)%>%
  summarise(MeanDeparture=n())%>%
  filter(MeanDeparture>100)%>%
  arrange(MeanDeparture)

f3<-flights%>%
  filter(carrier%in%c("00","YV"),
         !is.na(dep_time))%>%
  arrange(carrier,dep_time)%>%
  group_by(carrier)%>%
  mutate(PreviousDelay = lag(dep_delay))
  

