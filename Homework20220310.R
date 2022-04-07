#####Problem 1#####

i<- NULL
for (i in 1:10){
  FinalNumber<-i*3
  print(FinalNumber)
}


#####Problem 2#####

for (i in 1:10){
  RN<-rnorm(1,mean=0,sd=1)
  if(RN > 1){
    print(RN)
  }
}


#####Problem 3#####
# What is the probability that out of a group of 6 men and 8 women, if we pick
# 5 people at random, exactly 3 will be men?
# Use a for loop, which simulates the picking.


mw<-c(1,1,1,1,1,1,
      0,0,0,0,0,0,0,0)
pick<-NULL
result<-NULL

for (i in 1:5){
  pick<-sample(1:14,1)
  if (pick > 6){
    result=c(result,0)
  } else {
    result=c(result,1)
  }
}

ResultS<-sum(result)

All<-choose(14,5)

probability<-(ResultS/All)*100
P<-c*100

#####Problem 4#####
