################################################
### R Code for Lecture 4 - Estimating Population Properties
###   
###
### Last Modified 9/11/2012
###
### Changelog:
### 
################################################


## @knitr lecture4prep
set.seed(5000)
population<-runif(n = 400, min = 1, max = 100)

library(sn, quietly=TRUE)
library(ggplot2)


## @knitr lecture4-title
apop<-rnorm(5000, 15,5)
samp<-sapply(rep(1:100,5), function(x) mean(sample(apop, x)))

#two panels
par(mfrow=c(1,2))
plot(rep(1:100,5), samp, pch=19, xlab="Sample Size", ylab="Estimated Mean")
abline(mean(apop), 0, col="blue", lwd=5)

#boostrapped standard error
est<-sapply(rep(1:100), function(x) mean(sample(apop, 15)))

#draw the distributions of means with a bootstrapped 95% CI
hist(est, main="Esimated Means from 100 simulations", xlab="Mean with n=15", col="grey")
lines(rep(mean(est)+2*sd(est), 2), c(0,40), col="red", lwd=4)
lines(rep(mean(est)-2*sd(est), 2), c(0,40), col="red", lwd=4)

#set everything back to normal
par(mfrow=c(1,1))


## @knitr bootMean
#first create a vector of sample sizes
n<-rep(1:400, times = 4)

# next create an empty vector
m<-rep(NA, times = length(n))

#now a loop
for(i in 1:length(n)){
  m[i]<-mean(sample(population, size=n[i]))
}

#plot the result
plot(n, m, xlab="size", ylab="mean")


######For stepping through piece by piece
## @knitr bootMean1
n<-rep(1:400, times = 4)

## @knitr bootMean2
m<-rep(NA, times = length(n))

## @knitr bootMean3
for(i in 1:length(n)){
  m[i]<-mean(sample(population, size=n[i]))
}

## @knitr bootMean4
plot(n, m, xlab="size", ylab="mean")

## @knitr fib
#start with a blank vector with some 1's
fibVec<-c(1,1,rep(NA, 13))

#now loop
for(i in 3:15){
  fibVec[i] <- fibVec[i-1] + fibVec[i-2]  
}

fibVec

## @knitr fibBlank
#start with a blank vector, use ifelse to fill in first two
fibVec2 <- rep(NA, 15)
for(i in 1:15)  {
  fibVec[i] <- ifelse(i<3, 1, fibVec[i-1] + fibVec[i-2])
}
fibVec


## @knitr skew
samp<-seq(-5,5,.01)
plot(samp, dsn(samp), type="l", col="black", lwd=4, ylim=c(0,0.9), ylab="frequency", xlab="measure")
matplot(samp, dsn(samp, shape=4), type="l", col="red", add=T, lwd=4)

## @knitr kurtosis
plot(samp, dunif(samp, -5, 5), type="l", col="red", lwd=4, ylim=c(0,0.9), ylab="frequency", xlab="measure")
matplot(samp, dsn(samp), type="l", col="black", lwd=4, add=T)
matplot(samp, dsn(samp, scale=.5), type="l", col="blue", lwd=4, add=T)


## @knitr load-bird
bird<-read.csv("./data/02e1bDesertBirdCensus.csv")
birdDistPlot <- qplot(Count, data=bird, geom="histogram",fill=I("darkblue"), binwidth=20)+theme_bw()

## @knitr mode
birdDistPlot + annotate("text",100,20,label="Mode", color="red", size=8)


## @knitr median
birdDistPlot + 
  geom_vline(xintercept=median(bird$Count), lwd=3, colour="red") + 
  annotate("text", 195,15, label=paste("Median = ", median(bird$Count)), size=8)

## @knitr median2
sort(bird$Count)

nrow(bird) #this is the # of rows in the data frame

sort(bird$Count)[22]

## @knitr quantiles
sort(bird$Count)

## @knitr quartiles
boxplot(bird$Count, horizontal=T)

## @knitr sample
set.seed(345)
n<-400
qplot(runif(n,0,n), runif(n,0,n), size=runif(n,0,n), colour=runif(n,0,n)) + 
  theme_bw() +xlab("")+ylab("") +
  scale_color_gradientn(guide=FALSE, colours = c("grey", rainbow(15), "black")) + 
  scale_size_continuous(guide=FALSE, range=c(2,10))+
  geom_rect(aes(xmin=25, xmax=225, ymin=153, ymax=353), colour="red", lwd=1, fill=NA)


## @knitr CLT-unif-lab-single
set.seed(697)
n<-3
mvec<-rep(NA, times=100)

#simulate sampling events!
for(i in 1:length(mvec)){
  mvec[i]<-mean(runif(n, 0,100))
}

hist(mvec, main="n=3")

## @knitr CLT-unif-lab
set.seed(697)
n<-rep(seq(from=3, to=30, by = 3),70)
mvec<-rep(NA, times=length(n))

#simulate sampling events!
for(i in 1:length(n)){
  mvec[i]<-mean(runif(n[i], 0,100))
}

## @knitr CLT-unif-lab-plot
#now use a for loop to iterate over a few sample sizes and plot
#note, par sets the number of rows and columns in a plot
par(mfrow=c(2,2))
for(i in n[c(1,3,5,9)]){
  idx<-which(n==i)
  hist(mvec[idx], xlab="", ylab="", main=paste("n = ", i, sep=""), xlim=c(20,80))
}
par(mfrow=c(1,1))

## @knitr CLT-normal
set.seed(697)
mvec<-sapply(n, function(x) mean(rnorm(x, 7.5, 2)))
mdata<-data.frame(n=n, mvec=mvec)
qplot(mvec, geom="histogram", binwidth=0.3, facets=~n) + theme_bw()

## @knitr CLT-unif
set.seed(697)
mvec<-sapply(n, function(x) mean(runif(x, 0,100)))
mdata<-data.frame(n=n, mvec=mvec)
qplot(mvec, geom="histogram", binwidth=2, facets=~n, data=mdata) + theme_bw()


## @knitr CLT-gamma-median
set.seed(697)
mvec<-sapply(n, function(x) median(rgamma(x, shape=3)))
mdata<-data.frame(n=n, mvec=mvec)
qplot(mvec, geom="histogram", binwidth=0.2, facets=~n, data=mdata) + theme_bw()

## @knitr birds-sample
sample(bird$Count, replace=T, size=nrow(bird))

## @knitr birds-se-boot
n.sims<-100
birdMean <- rep(NA, n.sims)
for(i in 1:n.sims){
  birdMean[i] <- mean(sample(bird$Count, replace=T, size=nrow(bird)))
}
sd(birdMean)

## @knitr birds-se
sd(bird$Count)/sqrt(nrow(bird))

## @knitr CI
set.seed(697)
n<-20
upperCIvec<-rep(NA, n)
lowerCIvec<-rep(NA, n)

#loop and calculate the 95% CI
for(i in 1:n){
  samp<-rnorm(10)
  upperCIvec[i]<-mean(samp) + 2*sd(samp)/sqrt(n)
  lowerCIvec[i]<-mean(samp) - 2*sd(samp)/sqrt(n)
}

## @knitr CIShow
#examine the numbers
cbind(upperCIvec, lowerCIvec)[1:10,]

## @knitr CIPlot
#plot the CIs to show overlap
plot(0,0, xlim=c(-1.3,1.3), ylim=c(0,21), col="white", ylab="", xlab="value")
segments(upperCIvec, 1:n, lowerCIvec, 1:n, lwd=2)
lines( c(0,0), c(0,22), lwd=2, col="red")