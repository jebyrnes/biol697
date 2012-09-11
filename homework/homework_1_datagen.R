###-----------
# This script generates the bad data set for use
# in homework 1 regarding correcting data entry errors
#
# Jarrett Byrnes
# biol697
#
# Last updated 9/5/2012
###-----------

#set.seed ensures that you have the same random numbers everytime
#you run the full script
set.seed(2002)

#sample size
n<-500


#it all starts with chlorophyll and turbidity
waterClarity<-data.frame(
  site = rep(c('A', 'B', 'C', 'D', 'EE'), 100),
  turbidity = runif(n, 3,10),
  chlA = runif(n, 0, 5)
  
  )


waterClarity<-within(waterClarity, {
  
  zooplankton <- rnorm(n, 50*chlA + 20*turbidity, 15)
  
  
  
})



#now muck things up!

#NA strings that are not blank
waterClarity[round(runif(25,1,n)),2]<-"."

#outliers that sould probably be dropped
idx2<-round(runif(15,1,n))
waterClarity[idx2,3]<- waterClarity[idx2,3]+runif(15,20,100)

#biased values
idx3<-round(runif(75,1,n))
waterClarity[idx3,4]<- waterClarity[idx3,4]*1000


#bad site labels
#eIDX<-which(waterClarity[,1]=="E")
#idx4<-eIDX[round(runif(7,1,length(eIDX)))]

write.table(waterClarity, file="./ponds.txt", sep=" ", row.names=F)
