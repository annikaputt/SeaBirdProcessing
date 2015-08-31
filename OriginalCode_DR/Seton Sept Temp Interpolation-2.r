

makeTransparent<-function(someColor, alpha=75){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}



library(plyr)
library(colorRamps)
library(RColorBrewer)
require(grDevices)

stn.names<-c("pressure", "temperature", "distance")
#change file to s01

	so1<-read.csv(file ="Sept S01 depth temp.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
	so1$pressure<-round(so1$pressure, 1)   # increase to one decimal place to increase depth resolution 
	stn.1<-ddply(subset(so1, pressure>1), c("pressure"), summarize, pressure=mean(pressure), temperature=round(mean(temperature),2))
	
	dummy.pval.1<-data.frame(pressure=seq(from=0, max(stn.1$pressure), by=0.1))
	dummy.1<-merge(dummy.pval.1, stn.1, all.x=TRUE, by="pressure")
	
	stn1<-data.frame(approx(dummy.1$pressure, dummy.1$temperature, xout=dummy.1$pressure, method = "linear")[1],
		approx(dummy.1$pressure, dummy.1$temperature, xout=dummy.1$pressure, method = "linear")[2], 
		"distance"=1)
	colnames(stn1)<-stn.names
	stn1$temperature[is.na(stn1$temperature)] <- max(stn1$temperature, na.rm=TRUE)

	so2<-read.csv(file = "Sept S02 depth temp.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
	so2$pressure<-round(so2$pressure, 1)   # increase to one decimal place to increase depth resolution 
	stn.2<-ddply(subset(so2, pressure>1), c("pressure"), summarize, pressure=mean(pressure), temperature=round(mean(temperature),2))
	
	dummy.pval.2<-data.frame(pressure=seq(from=0, max(stn.2$pressure), by=0.1))
	dummy.2<-merge(dummy.pval.2, stn.2, all.x=TRUE, by="pressure")
	
	stn2<-data.frame(approx(dummy.2$pressure, dummy.2$temperature, xout=dummy.2$pressure, method = "linear")[1],
		approx(dummy.2$pressure, dummy.2$temperature, xout=dummy.2$pressure, method = "linear")[2], 
		"distance"=3.5)
	colnames(stn2)<-stn.names
	stn2$temperature[is.na(stn2$temperature)] <- max(stn2$temperature, na.rm=TRUE)
	
		
	so3<-read.csv(file = "Sept S03 depth temp.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
	so3$pressure<-round(so3$pressure, 1)   # increase to one decimal place to increase depth resolution 
	stn.3<-ddply(subset(so3, pressure>1), c("pressure"), summarize, pressure=mean(pressure), temperature=round(mean(temperature),2))
	
	dummy.pval.3<-data.frame(pressure=seq(from=0, max(stn.3$pressure), by=0.1))
	dummy.3<-merge(dummy.pval.3, stn.3, all.x=TRUE, by="pressure")
	
	stn3<-data.frame(approx(dummy.3$pressure, dummy.3$temperature, xout=dummy.3$pressure, method = "linear")[1],
		approx(dummy.3$pressure, dummy.3$temperature, xout=dummy.3$pressure, method = "linear")[2], 
		"distance"=7.6)
	colnames(stn3)<-stn.names
	stn3$temperature[is.na(stn3$temperature)] <- max(stn3$temperature, na.rm=TRUE)
	
	
	so4<-read.csv(file = "Sept S04 depth temp.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
	so4$pressure<-round(so4$pressure, 1)   # increase to one decimal place to increase depth resolution 
	stn.4<-ddply(subset(so4, pressure>1), c("pressure"), summarize, pressure=mean(pressure), temperature=round(mean(temperature),2))
	
	dummy.pval.4<-data.frame(pressure=seq(from=0, max(stn.4$pressure), by=0.1))
	dummy.4<-merge(dummy.pval.4, stn.4, all.x=TRUE, by="pressure")
	
	stn4<-data.frame(approx(dummy.4$pressure, dummy.4$temperature, xout=dummy.4$pressure, method = "linear")[1],
		approx(dummy.4$pressure, dummy.4$temperature, xout=dummy.4$pressure, method = "linear")[2], 
		"distance"=11.6)
	colnames(stn4)<-stn.names
	stn4$temperature[is.na(stn4$temperature)] <- max(stn4$temperature, na.rm=TRUE)
	
	
	
	so5<-read.csv(file = "Sept S05 depth temp.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
	so5$pressure<-round(so5$pressure, 1)   # increase to one decimal place to increase depth resolution 
	stn.5<-ddply(subset(so5, pressure>1), c("pressure"), summarize, pressure=mean(pressure), temperature=round(mean(temperature),2))
	
	dummy.pval.5<-data.frame(pressure=seq(from=0, max(stn.5$pressure), by=0.1))
	dummy.5<-merge(dummy.pval.5, stn.5, all.x=TRUE, by="pressure")
	
	stn5<-data.frame(approx(dummy.5$pressure, dummy.5$temperature, xout=dummy.5$pressure, method = "linear")[1],
		approx(dummy.5$pressure, dummy.5$temperature, xout=dummy.5$pressure, method = "linear")[2], 
		"distance"=17.0)
	colnames(stn5)<-stn.names
	stn5$temperature[is.na(stn5$temperature)] <- max(stn5$temperature, na.rm=TRUE)
	
	
	so6<-read.csv(file = "Sept S06 depth temp.csv", header = TRUE, stringsAsFactors = FALSE)[,-1]
	so6$pressure<-round(so6$pressure, 1)   # increase to one decimal place to increase depth resolution 
	stn.6<-ddply(subset(so6, pressure>1), c("pressure"), summarize, pressure=mean(pressure), temperature=round(mean(temperature),2))
	
	dummy.pval.6<-data.frame(pressure=seq(from=0, max(stn.6$pressure), by=0.1))
	dummy.6<-merge(dummy.pval.6, stn.6, all.x=TRUE, by="pressure")
	
	stn6<-data.frame(approx(dummy.6$pressure, dummy.6$temperature, xout=dummy.6$pressure, method = "linear")[1],
		approx(dummy.6$pressure, dummy.6$temperature, xout=dummy.6$pressure, method = "linear")[2], 
		"distance"=20.0)
	colnames(stn6)<-stn.names
	stn6$temperature[is.na(stn6$temperature)] <- max(stn6$temperature, na.rm=TRUE)



	stn.all<-rbind(stn1, stn2, stn3, stn4, stn5, stn6)
	head(stn.all)
	ddply(stn.all, c("distance"), summarize, max.pressure=max(pressure))
	#add in all other 

	sample.interval<-10
		dist.interval<-function(stn.x, stn.y){
				stnxy<-subset(merge(stn.x, stn.y, by="pressure", all.x=TRUE), temperature.y!="NA")
				stn.x<-stnxy[,c(1:3)]; colnames(stn.x)<-c("pressure", "temperature", "distance")
				stn.y<-stnxy[,c(1,4:5)]; colnames(stn.y)<-c("pressure", "temperature", "distance")
				rbind(stn.x, stn.y)
						}

	stn12a<-dist.interval(stn1, stn2)
	from<-min(stn12a$distance)*sample.interval #increase to increase resolution between stations
	to<-max(stn12a$distance)*sample.interval
	diff<-to-from
		for(i in (from+1):(to-1)){
				stn.temp<-data.frame(pressure=stn1$pressure, temperature=NA, distance=i/sample.interval)[-length(stn1$pressure),]
				stn12a<-rbind(stn12a, stn.temp)
						}

	stn12<-data.frame(stn12a, block=1)
	head(stn12)

	stn23a<-dist.interval(stn2, stn3)
	from<-min(stn23a$distance)*sample.interval
	to<-max(stn23a$distance)*sample.interval
	diff<-to-from
		for(i in (from+1):(to-1)){
				stn.temp<-data.frame(pressure=stn2$pressure, temperature=NA, distance=i/sample.interval)[-length(stn2$pressure),]
				stn23a<-rbind(stn23a, stn.temp)
						}

	stn23<-data.frame(stn23a, block=2)

	head(stn23)

	stn34a<-dist.interval(stn3, stn4)
	from<-min(stn34a$distance)*sample.interval
	to<-max(stn34a$distance)*sample.interval
	diff<-to-from
		for(i in (from+1):(to-1)){
				stn.temp<-data.frame(pressure=stn4$pressure, temperature=NA, distance=i/sample.interval)
				stn34a<-rbind(stn34a, stn.temp)
						}

	stn34<-data.frame(stn34a, block=3)


	stn45a<-dist.interval(stn4, stn5)
	from<-min(stn45a$distance)*sample.interval
	to<-max(stn45a$distance)*sample.interval
	diff<-to-from
		for(i in (from+1):(to-1)){
				stn.temp<-data.frame(pressure=stn5$pressure, temperature=NA, distance=i/sample.interval)
				stn45a<-rbind(stn45a, stn.temp)
						}

	stn45<-data.frame(stn45a, block=4)

	stn56a<-dist.interval(stn5, stn6)
	from<-min(stn56a$distance)*sample.interval
	to<-max(stn56a$distance)*sample.interval
	diff<-to-from
		for(i in (from+1):(to-1)){
				stn.temp<-data.frame(pressure=stn6$pressure, temperature=NA, distance=i/sample.interval)
				stn56a<-rbind(stn56a, stn.temp)
						}

	stn56<-data.frame(stn56a, block=5)

	stn<-rbind(stn12, stn23, stn34, stn45, stn56)

	estimate<-ddply(stn, c("block","pressure"), function(x){
  		dist<-max(x$distance)*sample.interval
  		estimate <- approx(x$distance, x$temperature, n=dist, method = "linear")
  		data.frame(estimate)
						})

	colnames(estimate)<-c("block", "pressure", "distance", "temperature")
	estimate$temp<-round(estimate$temperature, 1)




jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                 "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
jet.colors.matrix<-matrix(data=NA, ncol=2, nrow=22)
jet.colors.matrix[,1]<-seq(from=21,to=0, l=22)
jet.colors.matrix[,2]<-jet.colors(22)
jet.colors.matrix<- jet.colors.matrix[order(as.numeric(jet.colors.matrix[,1])),][,-1]

png("Sept 2014 Temp Profile.png",  width = 1920, height = 960, units = "px", pointsize = 16)      
layout(matrix(1:2,ncol=2), width = c(6,1),height = c(1,1))
par(mar=c(3,3,2,0), oma=c(2,2,3,1))

plot(pressure~distance, data=estimate, col=jet.colors(22)[findInterval(estimate$temp, seq(1:125))+1], pch=15, cex=1, ylim=rev(range(pressure)), axes=FALSE, las=1, xlab="", ylab="")
temp.value<-unique(findInterval(estimate$temp, seq(1:125)))+1

temp.code<-unique(jet.colors(22)[findInterval(estimate$temp, seq(1:125))+1])
temp.colour<-data.frame(temp.value, temp.code)
temp.colour<-temp.colour[order(temp.colour$temp.value, decreasing=TRUE),]

axis(1, line=-1.25, at=c(1,5,10,15,20), col=makeTransparent("black", 150))
axis(2, las=1, at=seq(0, max(estimate$pressure), by=5),labels=FALSE, line=-2.35, col=makeTransparent("black", 150))
mtext(text=seq(0,max(estimate$pressure),by=20), side=2, line=-1, at = seq(0,max(estimate$pressure), by=20), las=1)
axis(4, las=1, at=seq(0, max(estimate$pressure), by=5),labels=FALSE, line=-2.35, col=makeTransparent("black", 150))
mtext(text=seq(0,max(estimate$pressure),by=20), side=4, line=-1, at = seq(0,max(estimate$pressure), by=20), las=1)
mtext("Distance (km)", side=1, line=3, cex=1.5)
mtext("Depth (m)", side=2, cex=1.5, line=1)
mtext("inflow", side=1, adj=0, cex=1.25, line=2)
mtext("outflow", side=1, adj=1, cex=1.25, line=2)
text(x=1, y = -3, labels = "Stn 1")
lines(x=c(1,1), y=c(0, max(subset(estimate, block==1)$pressure)), lty=2, lwd=2, col="grey60")
text(x=3.5, y = -3, labels = "Stn 2")
lines(x=c(3.5,3.5), y=c(0, max(subset(estimate, block==2)$pressure)), lty=2, lwd=2, col="grey60")
text(x=7.6, y = -3, labels = "Stn 3")
lines(x=c(7.6,7.6), y=c(0, max(subset(estimate, block==2)$pressure)), lty=2, lwd=2, col="grey60")
text(x=11.6, y = -3, labels = "Stn 4")
lines(x=c(11.6,11.6), y=c(0, max(subset(estimate, block==3)$pressure)), lty=2, lwd=2, col="grey60")
text(x=17, y = -3, labels = "Stn 5")
lines(x=c(17,17), y=c(0, max(subset(estimate, block==4)$pressure)), lty=2, lwd=2, col="grey60")
text(x=20, y = -3, labels = "Stn 6")
lines(x=c(20,20), y=c(0, max(subset(estimate, block==5)$pressure)), lty=2, lwd=2, col="grey60")

legend_image <- as.raster(jet.colors.matrix)
plot(c(0,1.5),c(0,22),type = 'n', axes = F,xlab = '', ylab = '')
mtext(expression(paste("Temperature (",degree,"C)")), side=3, adj=0.95, cex=1.25, outer=TRUE, line=-2)
#axis(4, at=seq(min(turb.value),max(turb.value),l=5))
text(x=0.7, y = seq(0,22,l=9), labels = seq(0,22,l=9))

rasterImage(legend_image, -1, 0, 0.5,22)

dev.off()


  