###################################
#For Question 2
#First, read in the data
#note, we'll need to specify some na.strings, that there is a white space separating each column
#and that the header row contains the variable names
###################################

## @knitr homework1_2a-1
ponds<-read.table("../homework/ponds.txt", sep=" ", na.strings=".", header=T)

## @knitr homework1_2a-2
#####
#2a - this is the fastest method, using the arr.ind argument
which(is.na(ponds), arr.ind=T)
#####

#They are all in column 2, which is....
names(ponds)[2]

## @knitr homework1_2b
#####
#2b - fix the bad chl readings
#####
plot(ponds$chlA)
badChl<-which(ponds$chlA>20)
ponds$chlA[badChl,3]<-NA #other solutions possible

## @knitr homework1_2c
#####
#2c - fix the bad zooplankton data
#####
plot(ponds$zooplankton) #looks like the cutoff is ~ 1000
plot(ponds$zooplankton, ylim=c(0,10000))

#Now fix it using which statments
ponds$zooplankton[which(ponds$zooplankton>10000)]<-ponds$zooplankton[which(ponds$zooplankton>10000)] / 1000
plot(ponds$zooplankton) # Fixed!


## @knitr homework1_2d-1
#####
#Extra Credit
#####

#this won't work
ponds$site[which(ponds$site=="EE")]<-"E"

## @knitr reloadHW1Data
ponds<-read.table("../homework/ponds.txt", sep=" ", na.strings=".", header=T)

## @knitr homework1_2d-2
#instead, you need to do this...
ponds$site<-as.character(ponds$site)

ponds$site[which(ponds$site=="EE")]<-"E"

ponds$site<-factor(ponds$site)

## @knitr homework1_2d-3
#Or, just change the levels
levels(ponds$site)

levels(ponds$site) <- c("A", "B", "C", "D", "E")

levels(ponds$site)

## @knitr homework1_2d-4
# alternative approach
levels(ponds$site) <- c(levels(ponds$site)[1:4], "E")

ponds$site[1:10]


## @knitr homework1_exercise
# alternative approach
v1<-factor( rep(c("A", "B", "C", "D"), 10))

v2<-factor( rep(c("A1", "B1", "C1", "D1"), 10))

v3<-factor(c(as.character(v1), as.character(v2)))

v3[1:20]