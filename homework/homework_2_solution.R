
#2.1.1 SUM function for just one number
value<-0
for(i in 1:10){
  value<-value+i
}


#2.1.2 CUMSUM function for just one number
vec<-1:10
for(i in 2:10){
  vec[i]<-vec[i]+vec[i-1]
}


#2.2 More than a mean
#setup somem blank vectors
#
n<-rep(2:200,10)
meanVecN<-rep(NA, length(n))
sdVecN<-rep(NA, length(n))
medianVecN<-rep(NA, length(n))

meanVecU<-rep(NA, length(n))
sdVecU<-rep(NA, length(n))
medianVecU<-rep(NA, length(n))

#the loop!
for(i in 1:length(n)){
	
  #first, take samples from populations
  normSamp<-rnorm(n[i], 10, 5)
  USamp<-runif(n[i], 0,1000)

	#calculate summary statistics  
    meanVecN[i] <- mean(normSamp) 
    sdVecN[i] <- sd(normSamp)
    medianVecN[i] <- median(normSamp)
    meanVecU[i] <- mean(USamp)
    sdVecU[i] <- sd(USamp)
    medianVecU[i] <- median(USamp)
}

#plotting
#first use mfrow to make a 1 row x 3 col plot
par(mfrow=c(1,3))
plot(n, meanVecN)
plot(n, sdVecN)
plot(n, medianVecN)


plot(n, meanVecU)
plot(n, sdVecU)
plot(n, medianVecU)

#reset back to 1x1 for good hygiene
par(mfrow=c(1,1))
