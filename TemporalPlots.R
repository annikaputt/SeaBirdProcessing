############################
# TemporalPlots.R
# SeaBirdProcessing.Rproj

# Uses Dani's code (modified) to create heat maps of data

# Created August 31, 2015
# A Putt
#############################

library(lubridate)
library(plyr)

# Read in the data created in SeaBirdUpload.R
source("UploadStationDataCSV.R")

############################
# Prepare Data for Heatmaps
############################

# Standardize the pressures between all casts and perform linear interpolation to fill in any resulting gaps in desired variables

########################
# AveragingFunc
########################

# Standardize all pressures to follow the seq 0.1, 0.2, .... 
# Any missing variable data will be interpolated linearly from neighbouring data points
# This function is applied to one column (i.e., variable) from one cast (i.e., date)

averagingFunc <- function(column,data) {
  avgtemp <- data.frame(pressure.db=data$pressure.round,variable=data[,column]) # Retain pressure (rounded) and the variable of interest
  # There are many duplicate pressures associated with variable values. Find the average of the variable associated with each pressure.
  # The resulting frame has only one value of each pressure
  avgtemp.means <- ddply(subset(avgtemp, pressure.db > 1), c("pressure.db"), summarize, pressure.db=mean(pressure.db), variable=round(mean(variable),2))
  # Create a dummy vector of pressure with each 0.1 interval pressure value, then merge with the average data frame
  dummy.pressure <- data.frame(pressure.db=seq(from=0,to=max(avgtemp.means$pressure.db),by=0.1))
  avgtemp.dummy <- merge(dummy.pressure,avgtemp.means,all.x=TRUE,by="pressure.db")
  # Fill in the missing values that the merge created with a linear approximation and add a column for the day of the year  
  daynumber <- yday(data$posixdate[1]) # Identify the day number for each cast from the original oneday data frame created in the loop 
  avgfinal <-data.frame(
    approx(avgtemp.dummy$pressure.db, avgtemp.dummy$variable, xout=avgtemp.dummy$pressure.db, method = "linear")[1],
    approx(avgtemp.dummy$pressure.db, avgtemp.dummy$variable, xout=avgtemp.dummy$pressure.db, method = "linear")[2],"month"=daynumber)
  names(avgfinal) <- c("pressure.db","variable","month")
  # The linear approximation can leave NAs at the start of the data frame. Fill with the first whole value.
  avgfinal$variable[is.na(avgfinal$variable)] <- avgfinal$variable[max(which(is.na(avgfinal$variable)))+1]
  names(avgfinal) <- c("pressure.db",column,"month")
  return(avgfinal)
}


#####################
# AvgOneStationFunc
#####################

# Prepares station data to be run through the averaging function

AvgOneStationFunc <- function(StationDataFrame) {
  temp <- StationDataFrame
  temp$pressure.round <- round(temp$pressure,1) # Round the pressure to one decimal place to reduce the amount of data and to standardize
  temp$posixdate <- as.POSIXct(temp$date,format="%b %d %Y") # Create an actual date object

  # Subset an individual cast and perform minor manipulations
  # Create a list to put the data frames into
  templist <- list()
  turblist <- list()
  oxlist <- list()
  fluorlist <- list()
  for (i in (1:length(levels(temp$date)))) {
    onedate <- subset(temp,date==levels(temp$date)[i]) # Pull one date/cast
    # Find the day number from the original temp data frame (one station is subsetted)
    onedate.temp <- averagingFunc(column="temp.c",data=onedate) # Runs the function to create new a new data frame for one station and one date
    onedate.turb <- averagingFunc("turb.ntu",onedate)
    onedate.ox <- averagingFunc("oxygen.mg",onedate)
    onedate.fluor <- averagingFunc("fluor.mg.m3",onedate)
    # Put the data frames into the list
    templist[[i]] <- onedate.temp
    turblist[[i]] <- onedate.turb
    oxlist[[i]] <- onedate.ox
    fluorlist[[i]] <- onedate.fluor
  }
  # Bind the dates into one frame for each variable 
  AllTemp <- do.call("rbind",templist)
  AllTurb <- do.call("rbind",turblist)
  AllOx <- do.call("rbind",oxlist)
  AllFluor <- do.call("rbind",fluorlist)
  # browser()
  rownames(AllTemp) <- 1:nrow(AllTemp)
  rownames(AllTurb) <- 1:nrow(AllTurb)
  rownames(AllOx) <- 1:nrow(AllOx)
  rownames(AllFluor) <- 1:nrow(AllFluor)
  AllTemp$station <- temp$station[1]
  AllTurb$station <- temp$station[1]
  AllOx$station <- temp$station[1]
  AllFluor$station <- temp$station[1]
  
  # Put these three frames into one list
  OneStationList <- list(Temp=AllTemp,Turb=AllTurb,Ox=AllOx,Fluor=AllFluor)
  return(OneStationList)
}

###########################################
# Run averaging functions for all stations (loops)
###########################################
for (j in 1:length(file_path)) {
  onestationdata <- read.csv(sprintf("StationDataCSV/%s",file_path[j]),head=TRUE) # This pulls one station from the data and it a repeat from the start of this file
  name <- sprintf("Averaged%s",substr(file_path[j],start=1,stop=3))
  stationdata <- AvgOneStationFunc(onestationdata)
  assign(name,stationdata,envir=.GlobalEnv)
}
 
# The above loop has created a list with the name format "AveragedS1"
names(AveragedA01)
class(AveragedA01)
class(AveragedA01[[1]])
# You can double check that the code isn't accidentally writing over itself
ddply(AveragedA01$Temp, c("month"), summarize, max.temp=max(temp.c))


#################################
# oxygen.filldatesfunc
################################

# Looks like Dani filled in the missing days by adding all days between sampling dates to the month column
# Variables are NA for the extra days
# A block variable is added.

oxygen.filldatesfunc <- function(averageddata,sample.interval) {
  
  sampledates <- sort(unique(averageddata$month)) # Put the sample dates in sequential order
  from <- sampledates[1]
  to <- sampledates[length(sampledates)]
  diff <- to-from
  pressures <- subset(averageddata,month==sampledates[1])$pressure.db
  dummyoxygenlist <- list() # Create a data frame for pressure and oxygen for all dates between the min and max
  for (i in (from+1):(to-1)) {
    dummyoxygens <- data.frame(pressure.db=pressures,oxygen.mg=NA,month=i/sample.interval)[-length(pressures),] # I'm not sure why he has removed the final row, but I've replicated it here
    dummyoxygenlist[[i]] <- dummyoxygens
  }
  dummyoxygens <- do.call("rbind",dummyoxygenlist)
  
  # Need to get rid of any months that correspond to actual sampling dates
  for (i in 1:length(sampledates)) {
    dummyoxygens <- subset(dummyoxygens,month!=sampledates[i])
  }
  dummyoxygens$station <- averageddata$station[1]
  
  # Combine the dummy values with the sample dates
  alldatesox <- rbind(dummyoxygens,averageddata)
  
  # Add a block variable
  alldatesox$block <- NA
  blocks <- 1:(length(sampledates)-1)
  blockvector <- cut(alldatesox$month,breaks=c(sampledates[-length(sampledates)],Inf),labels=as.character(blocks),right=FALSE) 
  alldatesox$block <- blockvector # I'm not entirely sure if this worked properly.
  
  # In order to perform linear interpolations there needs to be two values for each pressure in each block
  # I need to add a duplicate set of data for the sample dates and give them a new block name so that each block
  # has more than one value to interpolate by.  
  for (i in 2:(length(sampledates)-1)) {
    temp <- subset(alldatesox,month==sampledates[i])
    temp$block <- i-1
    alldatesox <- rbind(alldatesox,temp)
  }
   
  # Need to get rid of NA values from the pressure in each block.
  newblocks <- list()
  for (i in 1:length(blocks)) { 
    block <- subset(alldatesox,block==blocks[i])
    block.values <- subset(block,oxygen.mg != "NA")
    days <- unique(block.values$month)
    pressures <- c(max(subset(block.values,month==days[1])$pressure.db),max(subset(block.values,month==days[2])$pressure.db))
    min.pressure <- min(pressures)
    newblock <- subset(block,pressure.db < min.pressure) # For some reason <= was glitching
    newblocks[[i]] <- newblock
  }
  
  alldatesox <- do.call("rbind",newblocks)  
    
  estimate<-ddply(subset(alldatesox,select=c(-station)), c("block","pressure.db"), function(x){ # Perform a linear interpolation to fill NA's
    mon <- max(x$month)*sample.interval
    estimate <- approx(x$month, x$oxygen.mg, n=mon, method = "linear") 
    data.frame(estimate) 
  })
  
  colnames(estimate)<-c("block", "pressure", "distance", "oxygen")
  estimate$oxy<-round(estimate$oxygen, 1)
  
  return(estimate)

}

#############################################################
# Run all of the stations through the oxygen.filldatesfunc
############################################################
# Only doing this for the stations that have more than one day. 
# Will have to fix this later on for the distance plots

StationList <- c("S01","S02","S03","S04","S05","S06","A01","A02","C01","C02","C03","C04","C05","C06","C07","C08","C09")
allstations <- list(AveragedS01,AveragedS02,AveragedS03,AveragedS04,AveragedS05,AveragedS06,AveragedA01,AveragedA02,AveragedC01,
                    AveragedC02,AveragedC03,AveragedC04,AveragedC05,AveragedC06,AveragedC07,AveragedC08,AveragedC09)
blockingloopnames <- sprintf("Averaged%s",StationList)
for (j in 1:length(StationList)) {
  name <- sprintf("blocked%s",StationList[j]) 
  toblock <- allstations[[j]] 
  oxygentoblock <- toblock$Ox
  blocked <- oxygen.filldatesfunc(oxygentoblock,1)
  assign(name,blocked,envir=.GlobalEnv)
}


####################################
# MakeTransparent Function
###################################


makeTransparent<-function(someColor, alpha=75){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
                                              blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}


####################################
# OxygenTimePlot
###################################


OxygenTimePlot <- function(estimate,averaged,original,ColourCount) {

  alldates <- sort(as.POSIXlt(levels(original$date),format="%b %d %Y"))
  allmonths <- sort(unique(averaged$month))
  station <- original$station[1]
  
  
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  jet.colors.matrix<-matrix(data=NA, ncol=2, nrow=ColourCount) # These vectors determine how many colours should be used in the plot
  jet.colors.matrix[,1]<-seq(from=ColourCount,to=0, l=ColourCount)
  jet.colors.matrix[,2]<-jet.colors(ColourCount)
  jet.colors.matrix<- jet.colors.matrix[order(as.numeric(jet.colors.matrix[,1])),][,-1]
  
  dates<-c(min(alldates),max(alldates))
  r<-range(estimate$distance)
  
  png(sprintf("Plots/%s.timeoxygen.png",station),  width = 1920, height = 960, units = "px", pointsize = 16)      
  layout(matrix(1:2,ncol=2), width = c(6,1),height = c(1,1))
  par(mar=c(3,3,2,0), oma=c(2,2,3,1))
  
  plot(pressure~distance, data=estimate, col=jet.colors(ColourCount)[findInterval(estimate$oxy, seq(1:125))+1], pch=15, cex=1, ylim=rev(range(pressure)), axes=FALSE, las=1, xlab="", ylab="")
  oxy.value<-unique(findInterval(estimate$oxy, seq(1:125)))+1  
  # browser() # At this browser you can check the oxy.value. The largest number has to correspond to the ColourCount.
  
  oxy.code<-unique(jet.colors(ColourCount)[findInterval(estimate$oxy, seq(1:125))+1])
  oxy.colour<-data.frame(oxy.value, oxy.code)
  oxy.colour<-oxy.colour[order(oxy.colour$oxy.value, decreasing=TRUE),]
  
  axis(1, line=-1.25, at=seq(r[1], r[2], by=1), labels=FALSE, col=makeTransparent("black", 150))
  axis(2, las=1, at=seq(0, max(estimate$pressure), by=5),labels=FALSE, line=-2.35, col=makeTransparent("black", 150))
  mtext(text=seq(0,max(estimate$pressure),by=20), side=2, line=-1, at = seq(0,max(estimate$pressure), by=20), las=1)
  axis(4, las=1, at=seq(0, max(estimate$pressure), by=5),labels=FALSE, line=-2.35, col=makeTransparent("black", 150))
  mtext(text=seq(0,max(estimate$pressure),by=20), side=4, line=-1, at = seq(0,max(estimate$pressure), by=20), las=1)
  mtext("Date", side=1, line=3, cex=1.5)
  mtext("Depth (m)", side=2, cex=1.5, line=1)
  mtext(alldates[1], side=1, at=allmonths[1], cex=1.25, line=1)
  mtext(alldates[2], side=1, at=allmonths[2], cex=1.25, line=1)
  mtext(alldates[3], side=1, at=allmonths[3], cex=1.25, line=1)
  mtext(alldates[4], side=1, at=allmonths[4], cex=1.25, line=1)
  mtext(alldates[5], side=1, at=allmonths[5], cex=1.25, line=1)
  mtext(alldates[6], side=1, at=allmonths[6], cex=1.25, line=1)
  
  lines(x=c(allmonths[1],allmonths[1]), y=c(0, max(subset(estimate, block==1)$pressure)), lty=2, lwd=2, col="grey60")
  lines(x=c(allmonths[2],allmonths[2]), y=c(0, max(subset(estimate, block==2)$pressure)), lty=2, lwd=2, col="grey60")
  lines(x=c(allmonths[3],allmonths[3]), y=c(0, max(subset(estimate, block==3)$pressure)), lty=2, lwd=2, col="grey60")
  lines(x=c(allmonths[4],allmonths[4]), y=c(0, max(subset(estimate, block==3)$pressure)), lty=2, lwd=2, col="grey60")
  lines(x=c(allmonths[5],allmonths[5]), y=c(0, max(subset(estimate, block==4)$pressure)), lty=2, lwd=2, col="grey60")
  lines(x=c(allmonths[6],allmonths[6]), y=c(0, max(subset(estimate, block==5)$pressure)), lty=2, lwd=2, col="grey60")
  
  legend_image <- as.raster(jet.colors.matrix)
  plot(c(0,1.5),c(0,ColourCount),type = 'n', axes = F,xlab = '', ylab = '')
  mtext(expression(paste("oxygen (mg/L)")), side=3, adj=0.95, cex=1.25, outer=TRUE, line=-2)
  #axis(4, at=seq(min(oxy.value),max(oxy.value),l=5))
  text(x=0.7, y = seq(0,ColourCount,l=9), labels = seq(0,ColourCount,l=9))
  
  rasterImage(legend_image, -1, 0, 0.5,ColourCount) 
  
  dev.off()

}

#################################
# Run the oxygen plot

OxygenTimePlot(blockedA01,AveragedA01$Ox,A01data,ColourCount=13)
OxygenTimePlot(blockedA02,AveragedA02$Ox,A02data,ColourCount=13)
OxygenTimePlot(blockedS01,AveragedS01$Ox,S01data,ColourCount=13)
OxygenTimePlot(blockedS02,AveragedS02$Ox,S02data,ColourCount=13)
OxygenTimePlot(blockedS03,AveragedS03$Ox,S03data,ColourCount=13)
OxygenTimePlot(blockedS04,AveragedS04$Ox,S04data,ColourCount=13)
OxygenTimePlot(blockedS05,AveragedS05$Ox,S05data,ColourCount=13)
OxygenTimePlot(blockedS06,AveragedS06$Ox,S06data,ColourCount=13)
OxygenTimePlot(blockedC01,AveragedC01$Ox,C01data,ColourCount=13)
OxygenTimePlot(blockedC02,AveragedC02$Ox,C02data,ColourCount=13)
OxygenTimePlot(blockedC03,AveragedC03$Ox,C03data,ColourCount=13)
OxygenTimePlot(blockedC04,AveragedC04$Ox,C04data,ColourCount=13)
OxygenTimePlot(blockedC05,AveragedC05$Ox,C05data,ColourCount=13)
OxygenTimePlot(blockedC06,AveragedC06$Ox,C06data,ColourCount=13)
OxygenTimePlot(blockedC07,AveragedC07$Ox,C07data,ColourCount=13)
OxygenTimePlot(blockedC08,AveragedC08$Ox,C08data,ColourCount=13)
OxygenTimePlot(blockedC09,AveragedC09$Ox,C09data,ColourCount=13)


