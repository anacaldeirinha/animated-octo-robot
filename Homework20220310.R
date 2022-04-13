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

ResultS<-sum(result)

All<-choose(14,5)


people<-c(rep("men",6),rep("women",8))
resultvector<-NULL

for(i in 1:1000){ 
  choose<-sample(people,size=4,replace=FALSE)
  if(sum(choose=="men")==3){
    resultvector<-c(resultvector,1)
  } else{
    resultvector<-c(resultvector,0)
  }
}

Sum<-sum(resultvector)

probability<-(Sum/1000)*100

#####Problem 4#####
