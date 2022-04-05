#####Problem 1#####

FactorialFunction <- function(inputNumber) {
  if (inputNumber == 0) {
    Result <- 1
  } else {
    Result <- inputNumber
    while(inputNumber > 1){
      Result <- (inputNumber - 1) * Result
      inputNumber <- inputNumber - 1
    }
  }
  return(Result)
}


#####Problem 2#####

  a <- c(12,16,13,15,18,9,2,20)
  
  SDFunction <- function(inputvector){
   Result<-sqrt(sum((a-mean(inputvector))^2/(length(inputvector)-1)))
return(Result)
  }

SDFunction(a)
sd(a)

  
#####Problem 3#####

nycflights13::flights

library(nycflights13)
library(tidyverse)


not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

# 5.6.7 Exercises 
#1.

#The flight delays are something crucial to analyse because if infuences
#directly the passagenrs lives. Despite that, the scenarios 1 and 3 demonstrate
# a delay or a early arrive in an inconsistent way. The passanger can?t have an
#idea of the time the flight will arrive.
#In the scenario 2, as the passanger know that the flight will always be 10 min 
#late, they can plan their lives already with that new arrival time.

#2.
not_cancelled %>%
  group_by(dest) %>%
  summarise(n = n())

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(n = sum(distance))

#3.
#The most important column is the arr_delay because a flight could depart and
# not arrive at the airport it was supposed to due to various reasons.

#4.


  cancelled_per_day <- 
    flights %>%
    mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
    group_by(year, month, day) %>%
    summarise(
      cancelled_num = sum(cancelled),
      flights_num = n(),
    )%>%
    arrange(desc(cancelled_num))

ggplot(cancelled_per_day) +
    geom_point(aes(x = flights_num, y = cancelled_num)) 

#As we can see through the graph, generally speaking, the number of cancelled 
#fligths increase with the number of flights per day.

#5.

not_cancelled %>% 
  group_by(carrier) %>% 
  summarise(arr_delay=mean(arr_delay))%>%
              arrange(desc(arr_delay))

#6.
#The argument sort let us decide if we want the data to return already sordered
#(TRUE) or not (FALSE).

# 5.7.1 Exercises

#2.
flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(arr_delay = mean(arr_delay), n = n()) %>%
  filter(n>=50)%>%
  filter(min_rank(desc(arr_delay)) == 1)

#3.

not_cancelled %>% 
  group_by(hour)%>%
  summarise(arr_delay=mean(arr_delay,na.rm=TRUE))%>%
  arrange(arr_delay)

#4.

not_cancelled_delayed <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay),arr_delay>0)

Delayed<-not_cancelled %>% 
  group_by(dest)%>%
  summarise(
    TotalDelay=sum(dep_delay),
    PropDelau = arr_delay/ TotalDelay
            )

#5.

#6.
aflights <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay),!is.na(air_time))

aaflights<-aflights%>%
  group_by(dest,origin)%>%
  mutate(
    mean = mean(air_time),
    air_sd = sd(air_time),
    n=n()
  )%>%
  mutate(air_time_standard = (air_time - mean)/(air_sd))
  
#7.
Destinatons<-not_cancelled%>%
  group_by(dest)%>%
  mutate(n_carriers=n_distinct(carrier))%>%
  filter(n_carriers>1)%>%
  group_by(carrier)%>%
  summarize(n_dest=n_distinct(dest))%>%
  arrange(desc(n_dest))

#8.


#####Problem 4#####


#4.1

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

delays <- not_cancelled %>% 
  group_by(carrier) %>% 
  summarise(
    mode = find_mode(dest))



#4.2

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

Biggest_delay <- not_cancelled %>% 
  group_by(carrier) %>% 
  summarise(
    most = max(dep_delay)
  )

#4.3
Most_Miles<-not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise( most = sum(distance),)%>%
   arrange(desc(most))%>%
  slice(1:3)


Least_Miles<-not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(least=min(distance))%>%
  arrange(least)%>%
  slice(1:3)


#4.4
not_cancelled2 <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay),year==2013,month==2)

FirstF<-not_cancelled2 %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time)
  )


LastF<-not_cancelled2 %>% 
  group_by(year, month, day) %>% 
  summarise(
    last = max(dep_time)
  )

#4.5
not_cancelled3 <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay),year==2013,month==3)

Least_Miles<-not_cancelled3 %>% 
  group_by(carrier) %>% 
  summarise(
    least_miles = sum(distance))%>%
  arrange(least_miles)%>%
  slice(1)
  

Most_Miles<-not_cancelled3 %>%
  group_by(carrier) %>% 
  summarise(
    most_miles = sum(distance))%>%
  arrange(desc(most_miles))%>%
  slice(1)
  

#4.6

not_cancelled4 <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay),dep_delay>60)

Delays<-not_cancelled4 %>%
  group_by(month) %>% 
  summarise(
    delayedd=n()
  )

#4.7

