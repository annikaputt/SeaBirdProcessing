############################
# TemporalPlots.R
# SeaBirdProcessing.Rproj

# Uses Dani's code (modified) to create heat maps of data

# Created August 31, 2015
# A Putt
#############################

library(lubridate)
library(plyr)

##########################
# Read in data
##########################

# Get the data manipulated in SeabirdUpload.R
# I read in the data separately for the averaging functions (from the same folder)
# Here is read in on it's own for plotting and such
# e.g., there will be a S1data (alldata) and AveragedS1 (rounded pressures)

# Option 1:
# Source the upload file so that all station data frames are in the working directory
# Right now the actual files are not retained in the workspace b/c they are generated in a function, so you have to run the 
# upload code and then read the data back in. But this is faster anyway because the upload takes a long time to run.
# source("SeaBirdUpload.R") 

# Option 2:
# If the upload has been performed and the most current files are in the StationDataCSV sub-folder you can just read those files in
file_path  <- list.files(path="StationDataCSV/", pattern="*.csv") # Pull all files
for(i in 1:length(file_path)){
  getdata <- read.csv(sprintf("StationDataCSV/%s",file_path[i]),head=TRUE)
  tempname <- substr(file_path[i],start=1,stop=6)
  assign(tempname,data.frame(getdata),envir=.GlobalEnv)  
}


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

AvgOneStationFunc <- function(StationDataFrame) {
  temp <- StationDataFrame
  temp$pressure.round <- round(temp$pressure,1) # Round the pressure to one decimal place to reduce the amount of data and to standardize
  temp$posixdate <- as.POSIXct(temp$date,format="%b %d %Y") # Create an actual date object

  # Subset an individual cast and perform minor manipulations
  # Create a list to put the data frames into
  templist <- list()
  turblist <- list()
  oxlist <- list()
  for (i in (1:length(levels(temp$date)))) {
    onedate <- subset(temp,date==levels(temp$date)[i]) # Pull one date/cast
    # Find the day number from the original temp data frame (one station is subsetted)
    onedate.temp <- averagingFunc(column="temp.c",data=onedate) # Runs the function to create new a new data frame for one station and one date
    onedate.turb <- averagingFunc("turb.ntu",onedate)
    onedate.ox <- averagingFunc("oxygen.mg",onedate)
    # Put the data frames into the list
    templist[[i]] <- onedate.temp
    turblist[[i]] <- onedate.turb
    oxlist[[i]] <- onedate.ox
  }
  # Bind the dates into one frame for each variable 
  AllTemp <- do.call("rbind",templist)
  AllTurb <- do.call("rbind",turblist)
  AllOx <- do.call("rbind",oxlist)
  # browser()
  rownames(AllTemp) <- 1:nrow(AllTemp)
  rownames(AllTurb) <- 1:nrow(AllTurb)
  rownames(AllOx) <- 1:nrow(AllOx)
  AllTemp$station <- temp$station[1]
  AllTurb$station <- temp$station[1]
  AllOx$station <- temp$station[1]
  
  # Put these three frames into one list
  OneStationList <- list(Temp=AllTemp,Turb=AllTurb,Ox=AllOx)
  return(OneStationList)
}

###########################################
# Run averaging functions for all stations
###########################################
for (j in 1:length(file_path)) {
  onestationdata <- read.csv(sprintf("StationDataCSV/%s",file_path[j]),head=TRUE) # This pulls one station from the data and it a repeat from the start of this file
  name <- sprintf("Averaged%s",substr(file_path[j],start=1,stop=2))
  stationdata <- AvgOneStationFunc(onestationdata)
  assign(name,stationdata,envir=.GlobalEnv)
}
 
# The above loop has created a list with the name format "AveragedS1"
names(AveragedS1)
class(AveragedS1)
class(AveragedS1[[1]])
# You can double check that the code isn't accidentally writing over itself
ddply(AveragedS1$Temp, c("month"), summarize, max.temp=max(temp.c))


#!!!!!!!!! EDITED UP TO HERE

######################################
######### Plotting Data ##############
######################################


##########################
# OneStationPlotFunc
##########################

# Code to plot data from an individual station (temp, oxygen, cond, turb, fluor)
# Creates a new page (window) for each cast (i.e., date)

OneStationPlotFunc <- function(StationDataFrame) {     # StationDataFrame is before the averaging manipulation. e.g., S1data, A1data, etc
  castonly <- subset(StationDataFrame,portion=="cast") # Remove surface and retrieval portions
  yaxisrange <- rev(range(castonly$pressure.db)) # Set the axes so that all of the graphs will be comparable
  temprange <- range(castonly$temp.c)
  condrange <- range(castonly$cond.us.cm3)
  turbrange <- range(castonly$turb.ntu)
  fluorrange <- range(castonly$fluor.mg.m3)
  oxrange <- range(castonly$oxygen.mg)
  for (i in 1:length(levels(castonly$date))) {   # Loop over each date/cast and create panel plot
    cast <- subset(castonly,date==levels(castonly$date)[i])
    windows()
    par(mfrow=c(2,3),oma=c(1,1,2,1))
    plot(cast$temp.c,cast$pressure.db,type="l",ylim=yaxisrange,xlim=temprange,ylab="Pressure dbar", xlab="Temp C")
    plot(cast$cond.us.cm3,cast$pressure.db,type="l",ylim=yaxisrange,xlim=condrange,ylab="Pressure dbar", xlab="Conductivity")
    plot(cast$turb.ntu,cast$pressure.db,type="l",ylim=yaxisrange,xlim=turbrange,ylab="Pressure dbar", xlab="Turbidity NTU")
    plot(cast$fluor.mg.m3,cast$pressure.db,type="l",ylim=yaxisrange,xlim=fluorrange,ylab="Pressure dbar", xlab="Fluorescence mg/m3")
    plot(cast$oxygen.mg,cast$pressure.db,type="l",ylim=yaxisrange,xlim=oxrange,ylab="Pressure dbar", xlab="Oxygen mg/L")
    mtext(sprintf("%s: %s",cast$station[1],cast$date[1]), side = 3, line=0, outer = TRUE)
  }
}

# Run example
# Will automatically open new plot windows; one for each date
OneStationPlotFunc(S1data)


#################################
# Heat Map Plots
################################

