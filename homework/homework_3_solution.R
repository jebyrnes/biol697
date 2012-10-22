###########################
##### Howework 3 - Graphics and Estimation Error
#####
##### Last modified 9/27/12
###########################

## @knitr hwprep
library(ggplot2)
library(bootstrap)
opts_chunk$set(out.height = "0.7\\textwidth")
opts_chunk$set(out.width = "0.7\\textwidth")


###########################2.1
## @knitr readAndClean
plankton<-read.csv("../lectures//data/hampton.5.1-Baikal_74_97_moAvg_plankton.csv", skip=1, na.strings=c("NA",  " NA", ".", " "))
plankton<-plankton[which(plankton$diatom<200),] #after visual inspection
plankton<-plankton[which(plankton$green<100),]#after visual inspection


###########################2.2a
## @knitr bootMedian
n.sims<-5000
diatomMedian<-rep(NA, n.sims)
for(i in 1:n.sims){
  diatomMedian[i]<-median(sample(plankton$diatom, size=nrow(plankton), replace=TRUE), na.rm=T)
}

diatomCIn<-2*sd(diatomMedian)
c(median(plankton$diatom)-diatomCIn, median(plankton$diatom)+diatomCIn)

###########################2.2b
quantile(diatomMedian, c(0.025, 0.975))



###########################2.2c
## @knitr bootMedian2
diatomMedian2<-bootstrap(plankton$diatom, 5000, median)
quantile(diatomMedian2$thetastar, c(0.025, 0.975))

###########################2.3
## @knitr facetPlotDinoByDiatom.ggplot
  qplot(diatom, dinoflagellate, data=plankton)+facet_wrap(~Month, scale="free") + 
  theme_bw() + 
  ggtitle("Relationship By Month")

## @knitr facetPlotDinoByDiatom
#set the # of panels
par(mfrow=c(3,4))

#loop over months, plotting on panel per month
for(a.month in unique(plankton$Month)){
  plot(dinoflagellate ~ diatom, data=plankton[which(plankton$Month == a.month),], pch=19, main=a.month)
}
title("Relationship By Month", outer=TRUE, line=-1.5)

#now be nice and reset plotting conditions
par(mfrow=c(1,1))


###########################2.4
## @knitr medianPlot1

#1) Create a new data frame that will have the information for plotting
#   using 
newPlankton<-data.frame(Month=unique(plankton$Month))

#2) For loop to calculate the aggregated properties
for(i in 1:nrow(newPlankton)){
  #3) Get the monthly data set
  shortDF<-plankton[which(plankton$Month==newPlankton$Month[i]),]
  
  #4) bootstrapped CIs
  shortDiatomMedian<-bootstrap(shortDF$diatom, 5000, median)
  
  newPlankton$Diatom.Median[i]<-median(shortDF$diatom)
  
  #6) Extract the monthly CIs
  newPlankton$Diatom.lowerCI[i]<-quantile(shortDiatomMedian$thetastar, 0.025)
  newPlankton$Diatom.upperCI[i]<-quantile(shortDiatomMedian$thetastar, 0.975)
  
}

#plot for points, segments for error bars
plot(Diatom.Median ~ Month, data=newPlankton, pch=19, ylim=c(0,15))
segments(newPlankton$Month, newPlankton$Diatom.lowerCI, newPlankton$Month, newPlankton$Diatom.upperCI)

## @knitr medianPlot2
#the ggplot2 way uses geom_point and geom_linerange
#although geom_pointrange would also work
  ggplot(data=newPlankton, aes(x=Month, y=Diatom.Median, ymin=Diatom.lowerCI, ymax=Diatom.upperCI)) +
  geom_point() +
  geom_linerange() +
  theme_bw()