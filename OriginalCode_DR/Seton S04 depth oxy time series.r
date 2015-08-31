######this is most current version### use this form now on- deals with near surface NA values-------------------

makeTransparent<-function(someColor, alpha=75){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

strptime(date, format=%Y-%m-%d)

library(plyr)
library(colorRamps)
library(RColorBrewer)
require(grDevices)

mon.names<-c("pressure", "oxygen", "month")

May<-read.csv(file ="Aug S01 depth oxy.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
May$pressure<-round(May$pressure, 1)   # increase to one decimal place to increase depth resolution 
mon.1<-ddply(subset(May, pressure>1), c("pressure"), summarize, pressure=mean(pressure), oxygen=round(mean(oxygen),2))

dummy.pval.1<-data.frame(pressure=seq(from=0, max(mon.1$pressure), by=0.1))
dummy.1<-merge(dummy.pval.1, mon.1, all.x=TRUE, by="pressure")

month1<-data.frame(approx(dummy.1$pressure, dummy.1$oxygen, xout=dummy.1$pressure, method = "linear")[1],
                 approx(dummy.1$pressure, dummy.1$oxygen, xout=dummy.1$pressure, method = "linear")[2], 
                 "month"=142)
colnames(month1)<-mon.names

month1$oxygen[is.na(month1$oxygen)] <- month1$oxygen[max(which(is.na(month1$oxygen)))+1]



June<-read.csv(file ="June S04 depth oxy.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
June$pressure<-round(June$pressure, 1)   # increase to one decimal place to increase depth resolution 
mon.2<-ddply(subset(June, pressure>1), c("pressure"), summarize, pressure=mean(pressure), oxygen=round(mean(oxygen),2))

dummy.pval.2<-data.frame(pressure=seq(from=0, max(mon.2$pressure), by=0.1))
dummy.2<-merge(dummy.pval.2, mon.2, all.x=TRUE, by="pressure")

month2<-data.frame(approx(dummy.2$pressure, dummy.2$oxygen, xout=dummy.2$pressure, method = "linear")[1],
                 approx(dummy.2$pressure, dummy.2$oxygen, xout=dummy.2$pressure, method = "linear")[2], 
                 "month"=162)
colnames(month2)<-mon.names
month2$oxygen[is.na(month2$oxygen)] <- month2$oxygen[max(which(is.na(month2$oxygen)))+1]

July<-read.csv(file ="July S04 depth oxy.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
July$pressure<-round(July$pressure, 1)   # increase to one decimal place to increase depth resolution 
mon.3<-ddply(subset(July, pressure>1), c("pressure"), summarize, pressure=mean(pressure), oxygen=round(mean(oxygen),2))

dummy.pval.3<-data.frame(pressure=seq(from=0, max(mon.3$pressure), by=0.1))
dummy.3<-merge(dummy.pval.3, mon.3, all.x=TRUE, by="pressure")

month3<-data.frame(approx(dummy.3$pressure, dummy.3$oxygen, xout=dummy.3$pressure, method = "linear")[1],
                 approx(dummy.3$pressure, dummy.3$oxygen, xout=dummy.3$pressure, method = "linear")[2], 
                 "month"=196)
colnames(month3)<-mon.names
month3$oxygen[is.na(month3$oxygen)] <- month3$oxygen[max(which(is.na(month3$oxygen)))+1]


Aug<- read.csv(file = "Aug S04 depth oxy.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
Aug$pressure<-round(Aug$pressure, 1)   # increase to one decimal place to increase depth resolution 
mon.4<-ddply(subset(Aug, pressure>1), c("pressure"), summarize, pressure=mean(pressure), oxygen=round(mean(oxygen),2))

head(mon.4)

     
dummy.pval.4<-data.frame(pressure=seq(from=0, max(mon.4$pressure), by=0.1))
dummy.4<-merge(dummy.pval.4, mon.4, all.x=TRUE, by="pressure")

month4<-data.frame(approx(dummy.4$pressure, dummy.4$oxygen, xout=dummy.4$pressure, method = "linear")[1],
                 approx(dummy.4$pressure, dummy.4$oxygen, xout=dummy.4$pressure, method = "linear")[2], 
                 "month"=232)
colnames(month4)<-mon.names
month4$oxygen[is.na(month4$oxygen)] <- month4$oxygen[max(which(is.na(month4$oxygen)))+1]

is.numeric(month4[,2])

Sept<- read.csv(file = "Sept S04 depth oxy.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
Sept$pressure<-round(Sept$pressure, 1)   # increase to one decimal place to increase depth resolution 
mon.5<-ddply(subset(Sept, pressure>1), c("pressure"), summarize, pressure=mean(pressure), oxygen=round(mean(oxygen),2))

dummy.pval.5<-data.frame(pressure=seq(from=0, max(mon.5$pressure), by=0.1))
dummy.5<-merge(dummy.pval.5, mon.5, all.x=TRUE, by="pressure")

month5<-data.frame(approx(dummy.5$pressure, dummy.5$oxygen, xout=dummy.5$pressure, method = "linear")[1],
                   approx(dummy.5$pressure, dummy.5$oxygen, xout=dummy.5$pressure, method = "linear")[2], 
                   "month"=267)
colnames(month5)<-mon.names
month5$oxygen[is.na(month5$oxygen)] <- month5$oxygen[max(which(is.na(month5$oxygen)))+1]



Oct<- read.csv(file = "Oct S04 depth oxy.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
Oct$pressure<-round(Oct$pressure, 1)   # increase to one decimal place to increase depth resolution 
mon.6<-ddply(subset(Oct, pressure>1), c("pressure"), summarize, pressure=mean(pressure), oxygen=round(mean(oxygen),2))

dummy.pval.6<-data.frame(pressure=seq(from=0, max(mon.6$pressure), by=0.1))
dummy.6<-merge(dummy.pval.6, mon.6, all.x=TRUE, by="pressure")

month6<-data.frame(approx(dummy.6$pressure, dummy.6$oxygen, xout=dummy.6$pressure, method = "linear")[1],
                   approx(dummy.6$pressure, dummy.6$oxygen, xout=dummy.6$pressure, method = "linear")[2], 
                   "month"=296)
colnames(month6)<-mon.names
month6$oxygen[is.na(month6$oxygen)] <- month6$oxygen[max(which(is.na(month6$oxygen)))+1]


stn.all<-rbind(month1,month2,month3,month4,month5,month6)
head(stn.all)
ddply(stn.all, c("month"), summarize, max.pressure=max(pressure))
#add in all other 

sample.interval<-1
month.interval<-function(month.x, month.y){
  monthxy<-subset(merge(month.x, month.y, by="pressure", all.x=TRUE), oxygen.y!="NA")
  month.x<-monthxy[,c(1:3)]; colnames(month.x)<-c("pressure", "oxygen", "month")
  month.y<-monthxy[,c(1,4:5)]; colnames(month.y)<-c("pressure", "oxygen", "month")
  rbind(month.x, month.y)
}

month12a<-month.interval(month1, month2)
from<-min(month12a$month)*sample.interval #increase to increase resolution between stations
to<-max(month12a$month)*sample.interval
diff<-to-from
for(i in (from+1):(to-1)){
  month.oxy<-data.frame(pressure=month1$pressure, oxygen=NA, month=i/sample.interval)[-length(month1$pressure),]
  month12a<-rbind(month12a, month.oxy)
}

month12<-data.frame(month12a, block=1)
head(month12)

month23a<-month.interval(month2, month3)
from<-min(month23a$month)*sample.interval
to<-max(month23a$month)*sample.interval
diff<-to-from
for(i in (from+1):(to-1)){
  month.oxy<-data.frame(pressure=month2$pressure, oxygen=NA, month=i/sample.interval)[-length(month2$pressure),]
  month23a<-rbind(month23a, month.oxy)
}

month23<-data.frame(month23a, block=2)

head(month23)

month34a<-month.interval(month3, month4)
from<-min(month34a$month)*sample.interval
to<-max(month34a$month)*sample.interval
diff<-to-from
for(i in (from+1):(to-1)){
  month.oxy<-data.frame(pressure=month3$pressure, oxygen=NA, month=i/sample.interval)[-length(month3$pressure),]
  month34a<-rbind(month34a, month.oxy)
}

month34<-data.frame(month34a, block=3)

head(month34)


month45a<-month.interval(month4, month5)
from<-min(month45a$month)*sample.interval
to<-max(month45a$month)*sample.interval
diff<-to-from
for(i in (from+1):(to-1)){
  month.oxy<-data.frame(pressure=month4$pressure, oxygen=NA, month=i/sample.interval)[-length(month4$pressure),]
  month45a<-rbind(month45a, month.oxy)
}

month45<-data.frame(month45a, block=4)

head(month45)


month56a<-month.interval(month5, month6)
from<-min(month56a$month)*sample.interval
to<-max(month56a$month)*sample.interval
diff<-to-from
for(i in (from+1):(to-1)){
  month.oxy<-data.frame(pressure=month5$pressure, oxygen=NA, month=i/sample.interval)[-length(month5$pressure),]
  month56a<-rbind(month56a, month.oxy)
}

month56<-data.frame(month56a, block=5)

head(month56)



summary(month12)

unique(month12$oxygen)

summary(month23)
unique(month23$oxygen)
month23$oxygen

summary(month34)
summary(month45)
summary(month56)


stn<-rbind(month12, month23, month34, month45, month56)

estimate<-ddply(stn, c("block","pressure"), function(x){
  mon<-max(x$month)*sample.interval
  estimate <- approx(x$month, x$oxygen, n=mon, method = "linear")
  data.frame(estimate)
})

colnames(estimate)<-c("block", "pressure", "distance", "oxygen")
estimate$oxy<-round(estimate$oxygen, 1)

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
jet.colors.matrix<-matrix(data=NA, ncol=2, nrow=15)
jet.colors.matrix[,1]<-seq(from=15,to=0, l=15)
jet.colors.matrix[,2]<-jet.colors(15)
jet.colors.matrix<- jet.colors.matrix[order(as.numeric(jet.colors.matrix[,1])),][,-1]

dates<-c("2014-05-22", "2014-10-23")
r<-range(estimate$distance)


png("S04 May-Oct 2014 oxy Profile.png",  width = 1920, height = 960, units = "px", pointsize = 16)      
layout(matrix(1:2,ncol=2), width = c(6,1),height = c(1,1))
par(mar=c(3,3,2,0), oma=c(2,2,3,1))

plot(pressure~distance, data=estimate, col=jet.colors(15)[findInterval(estimate$oxy, seq(1:125))+1], pch=15, cex=1, ylim=rev(range(pressure)), axes=FALSE, las=1, xlab="", ylab="")
oxy.value<-unique(findInterval(estimate$oxy, seq(1:125)))+1

oxy.code<-unique(jet.colors(15)[findInterval(estimate$oxy, seq(1:125))+1])
oxy.colour<-data.frame(oxy.value, oxy.code)
oxy.colour<-oxy.colour[order(oxy.colour$oxy.value, decreasing=TRUE),]

axis(1, line=-1.25, at=seq(r[1], r[2], by=1), labels=FALSE, col=makeTransparent("black", 150))
axis(2, las=1, at=seq(0, max(estimate$pressure), by=5),labels=FALSE, line=-2.35, col=makeTransparent("black", 150))
mtext(text=seq(0,max(estimate$pressure),by=20), side=2, line=-1, at = seq(0,max(estimate$pressure), by=20), las=1)
axis(4, las=1, at=seq(0, max(estimate$pressure), by=5),labels=FALSE, line=-2.35, col=makeTransparent("black", 150))
mtext(text=seq(0,max(estimate$pressure),by=20), side=4, line=-1, at = seq(0,max(estimate$pressure), by=20), las=1)
mtext("Date", side=1, line=3, cex=1.5)
mtext("Depth (m)", side=2, cex=1.5, line=1)
mtext("22-May", side=1, at=142, cex=1.25, line=1)
mtext("11-June", side=1, at=162, cex=1.25, line=1)
mtext("15-July", side=1, at=196, cex=1.25, line=1)
mtext("20-August", side=1, at=232, cex=1.25, line=1)
mtext("24-September", side=1, at=267, cex=1.25, line=1)
mtext("23-October", side=1, at=296, cex=1.25, line=1)

lines(x=c(142,142), y=c(0, max(subset(estimate, block==1)$pressure)), lty=2, lwd=2, col="grey60")

lines(x=c(162,162), y=c(0, max(subset(estimate, block==2)$pressure)), lty=2, lwd=2, col="grey60")

lines(x=c(196,196), y=c(0, max(subset(estimate, block==3)$pressure)), lty=2, lwd=2, col="grey60")

lines(x=c(232,232), y=c(0, max(subset(estimate, block==3)$pressure)), lty=2, lwd=2, col="grey60")

lines(x=c(267,267), y=c(0, max(subset(estimate, block==4)$pressure)), lty=2, lwd=2, col="grey60")

lines(x=c(296,296), y=c(0, max(subset(estimate, block==5)$pressure)), lty=2, lwd=2, col="grey60")

legend_image <- as.raster(jet.colors.matrix)
plot(c(0,1.5),c(0,15),type = 'n', axes = F,xlab = '', ylab = '')
mtext(expression(paste("oxygen (mg/L)")), side=3, adj=0.95, cex=1.25, outer=TRUE, line=-2)
#axis(4, at=seq(min(oxy.value),max(oxy.value),l=5))
text(x=0.7, y = seq(0,15,l=9), labels = seq(0,15,l=9))

rasterImage(legend_image, -1, 0, 0.5,15)


dev.off()











