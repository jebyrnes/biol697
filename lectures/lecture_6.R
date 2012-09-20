## @knitr lecture6prep
library(ggplot2)
par(mar=c(5,4,2,1)+0.1)

## @knitr readPlankton
plankton<-read.csv("./data/hampton.5.1-Baikal_74_97_moAvg_plankton.csv", skip=1, na.strings=c("NA",  " NA", ".", " "))
plankton$Snow <- plankton$Mean.snow.depth.Irkutsk...boulder.Snow...Ice.data
#summary(plankton)
#names(plankton)
plankton<-plankton[which(plankton$diatom<200),]


## @knitr showData
plot(Snow ~ Month, data=plankton)

## @knitr mimizeDistraction1
plot(Snow ~ Month, data=plankton, ylab="Mean Snow Depth")

## @knitr mimizeDistraction2
plot(Snow ~ Month, data=plankton, xlab="Mean Snow Depth", bty="n")

## @knitr differentPieces1
ggplot(data=plankton, aes(x=Month, y=Snow))  + 
  geom_line() +
  facet_wrap(~Year) +
  ylab("Mean Snow Depth") +
  theme_bw() +
  scale_x_continuous(breaks=c(3,6,9,12))

## @knitr differentPieces2
snowByMonthYear <- ggplot(data=plankton, aes(x=Year, y=Snow)) + 
  geom_line() +
  facet_wrap(~Month) +
  theme_bw() +
  ylab("Mean Snow Depth\n") +
  ggtitle("Mean Snow by Month Across Years")

snowByMonthYear

## @knitr stats
snowByMonthYear + stat_smooth(method="lm")

## @knitr statsClean
snowByMonthYear + stat_smooth(method="lm", color="red", alpha=0.2) +
  theme(strip.background=element_blank(), axis.line=element_blank(), panel.grid=element_blank(), axis.ticks=element_blank())


## @knitr boxplotDiatom
bpDiatom<-qplot(Month, diatom, data=plankton, geom="boxplot", group=Month) + ylim(0,30) +
  theme_bw() +
  theme(strip.background=element_blank(), 
        axis.line=element_blank(), 
        panel.grid=element_blank(), 
        axis.ticks=element_blank()) +
   ylab("Diatom Abundance\n")

bpDiatom 

## @knitr tufteBoxplot
#based on http://stackoverflow.com/questions/6973394/functions-available-for-tufte-boxplots-in-r
bpDiatomData<-ddply(plankton, .(Month), function(adf) return(boxplot.stats(adf$diatom)$stats))

tBoxPlot <- qplot(Month, diatom, data=plankton, geom="point", group=Month) + ylim(0,34) +
  geom_boxplot(fill="white", color="white", alpha=1, size=8) +
  theme_bw() +
  theme(strip.background=element_blank(), axis.line=element_blank(), panel.grid=element_blank(), axis.ticks=element_blank()) +
  geom_segment(data=bpDiatomData, aes(xend=Month, x=Month, y=V1, yend=V2), size=2) +
  geom_segment(data=bpDiatomData, aes(xend=Month, x=Month, y=V4, yend=V5), size=2) +
  ylab("Diatom Abundance\n")

tBoxPlot+  geom_segment(data=bpDiatomData, aes(xend=Month, x=Month, y=V2, yend=V3), size=1, color="grey") +
  geom_segment(data=bpDiatomData, aes(xend=Month, x=Month, y=V3, yend=V4), size=1, color="grey") +
  geom_point(data = bpDiatomData, aes(y = V3), size = 4, color="red")  

## 
## @knitr tufteBoxplotPure
#based on http://stackoverflow.com/questions/6973394/functions-available-for-tufte-boxplots-in-r

tBoxPlot + geom_point(data = bpDiatomData, aes(y = V3), size = 4, color="red")

## @knitr comment1
###########################
# plotting
##########################

## @knitr pairs
pairs(plankton)

## @knitr pairs2
pairs(plankton[,14:18])


## @knitr hist
hist(plankton$Snow)

## @knitr basicPlot1
plot(Copepod.total ~ diatom, data=plankton)


## @knitr basicPlot1a
plot(plankton$diatom, plankton$Copepod.total)


## @knitr basicPlot2
plot(Copepod.total ~ diatom, 
     data=plankton,
     xlab="Diatom Abundance", 
     ylab = "Copepod Abundnace")


## @knitr basicPlot2
plot(Copepod.total ~ diatom, data=plankton,
     xlab="Diatom Abundance", 
     ylab = "Copepod Abundnace",
     xlim=c(0,20))


## @knitr basicPlot3
plot(Copepod.total ~ diatom, data=plankton,
     xlab="Diatom Abundance", 
     ylab = "Copepod Abundnace",
     xlim=c(0,20), 
     pch=19)


## @knitr pchShow
pchShow <-
  function(extras = c("*",".", "o","O","0","+","-","|","%","#"),
           cex = 3, ## good for both .Device=="postscript" and "x11"
           col = "red3", bg = "gold", coltext = "brown", cextext = 1.2,
           main = paste("plot symbols :  points (...  pch = *, cex =",
                        cex,")"))
  {
    nex <- length(extras)
    np  <- 26 + nex
    ipch <- 0:(np-1)
    k <- floor(sqrt(np))
    dd <- c(-1,1)/2
    rx <- dd + range(ix <- ipch %/% k)
    ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
    pch <- as.list(ipch) # list with integers & strings
    if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
    plot(rx, ry, type="n", axes = FALSE, xlab = "", ylab = "",
         main = main)
    abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
    for(i in 1:np) {
      pc <- pch[[i]]
      ## 'col' symbols with a 'bg'-colored interior (where available) :
      points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
      if(cextext > 0)
        text(ix[i] - 0.3, iy[i], pc, col = coltext, cex = cextext)
    }
  }

pchShow()


## @knitr bigPointPlot
plot(Copepod.total ~ diatom, data=plankton,
     xlab="Diatom Abundance", ylab = "Copepod Abundnace",
     xlim=c(0,20), pch=19, cex=4)

## @knitr colorPlot
plot(Copepod.total ~ diatom, data=plankton,
     xlab="Diatom Abundance", ylab = "Copepod Abundnace",
     xlim=c(0,20), pch=19, col=Month)

## @knitr colorPlot2
plot(Copepod.total ~ diatom, data=plankton,
     xlab="Diatom Abundance", ylab = "Copepod Abundnace",
     xlim=c(0,20), pch=19, col=heat.colors(12)[Month])


## @knitr parPlot
par(mfrow=c(1,2))

#plot Copepods as the response
plot(Copepod.total ~ diatom, data=plankton,
     xlab="Diatom Abundance", ylab = "Copepod Abundnace",
     xlim=c(0,20), pch=19, col=heat.colors(12)[Month])

#now plot Daphnia
plot(Bosmina...Daphnia ~ diatom, data=plankton,
     xlab="Diatom Abundance", ylab = "Daphnia Abundnace",
     xlim=c(0,20), pch=19, col=heat.colors(12)[Month])
par(mfrow=c(1,1))

## @knitr otherPlotfunctions
?matplot
?lines
?axis
?title
?legend
?points
?segments

## @knitr lines
par(mfrow=c(5,5))
for(j in unique(plankton$Year)){
  plot(diatom ~ Month, data=subset(plankton, plankton$Year==j), type="l")
}
par(mfrow=c(1,1))


## @knitr lines2
par(mfrow=c(5,5), mar=c(4,4,1,1))
for(j in unique(plankton$Year)){
  plot(diatom ~ Month, data=subset(plankton, plankton$Year==j), type="l",
       xlab="", ylab="", main=j)
}
par(mfrow=c(1,1))
title(xlab="Month", ylab="Diatom Abundance")

## @knitr comment2
###########################
# ggplot2
# http://www.slideshare.net/dataspora/a-survey-of-r-graphics
##########################

## @knitr ggplot
p<-ggplot(data=plankton, mapping=aes(x=Month, y=Copepod.total))

p

## @knitr geompoint
p<-p+geom_point()
p

## @knitr prettyggplot
p<- p+ylab("Total Copepod Abundance") + theme_bw()
p


## @knitr color
p2<-ggplot(data=plankton,aes(x=Month, y=Copepod.total, color=Year))  + 
  geom_point() +
  theme_bw()
p2

## @knitr color2
p2<- p2 + scale_color_gradient(low="blue", high="red") 

p2

## @knitr color3
p2<- p2 + geom_line(aes(group=Year)) 
p2

## @knitr facet
p2 <- p2 + facet_wrap(~Year) +scale_x_continuous(breaks=c(3,6,9,12))
p2

## @knitr barplot
qplot(factor(Year), diatom, geom="bar", fill=factor(Month), data=plankton) +
  theme_bw() +
  xlab("Year") + ylab("Diatom Abundance\n") +
  scale_fill_discrete(name="Month")+
  scale_x_discrete(breaks=seq(1974, 1997, 5))


## @knitr otherggplot2
?theme
?labs
?xlim
?facet_grid
?scale_x_log10
?geom_histogram
?geom_ribbon
?geom_linerange
?geom_freqpoly
