############################
# TemporalPlots.R
# SeaBirdProcessing.Rproj

# Uses Dani's code (modified) to create heat maps of data

# Created August 31, 2015
# A Putt
#############################

# For now I need to re-run the upload file each time I want to plot
# returns a data fram for each station "S1data", "S2data"...."C10data", etc
source("SeaBirdUpload.R")

library(lubridate)
library(plyr)

##########################
# Code tidbits
##########################

# Code to plot data from an individual cast
# Subset a date from a station data frame
# Subset only the cast (get rid of the retrival and the surface portions
# Create plots of temp, oxygen, cond, turb, fluor
# for now I'll just do it for S1 data
A <- subset(S1data,date == "Aug 11 2015")
B <- subset(A,portion == "cast")
par(mfrow=c(2,3))
plot(B$temp.c,B$pressure.db,type="l",ylim=rev(range(B$pressure.db)),ylab="Pressure dbar", xlab="Temp C")
plot(B$cond.us.cm3,B$pressure.db,type="l",ylim=rev(range(B$pressure.db)),ylab="Pressure dbar", xlab="Conductivity")
plot(B$turb.ntu,B$pressure.db,type="l",ylim=rev(range(B$pressure.db)),ylab="Pressure dbar", xlab="Turbidity NTU")
plot(B$fluor.mg.m3,B$pressure.db,type="l",ylim=rev(range(B$pressure.db)),ylab="Pressure dbar", xlab="Fluorescence mg/m3")
plot(B$oxygen.mg,B$pressure.db,type="l",ylim=rev(range(B$pressure.db)),ylab="Pressure dbar", xlab="Oxygen mg/L")

############################
# Code to prepare data for heatmaps
# this represents the code in the start of the seton s04 depth oxy time series plot file
# Need to clean, automate, and apply to a bunch of data

# Turn date into a factor 
temp <- S1data #for now...
temp$posixdate <- as.POSIXct(temp$date,format="%b %d %Y")
temp$datefactor <- as.factor(temp$date)
temp$pressure.round <- round(temp$pressure,1)

# subset out each date !!! this will go into a function
tosubset <- levels(temp$datefactor)
temp <- subset(temp, datefactor==tosubset[1])

# Find the day number for the date
daynumber <- yday(temp$posixdate[1])
#temp$month <- daynumber

###############################################################################
# Create subsets for each of the variables I'm interested in plotting
# This will be working on a single cast subsetted from the single day (will be within another function)
variables <- c("temp.c","turb.ntu","oxygen.mg")
averagingFunc <- function(column) {
  # Subset only pressure and the variable of interest and round the pressure
  avgtemp <- data.frame(pressure.db=temp$pressure.round,variable=temp[,column])
  # Find the average of the variable for each value of pressure
  avgtemp.means <- ddply(subset(avgtemp, pressure.db > 1), c("pressure.db"), summarize, pressure.db=mean(pressure.db), variable=round(mean(variable),2))
  # Create a dummy vector of pressure with each 0.1 interval pressure value, then merge with the average data frame
  dummy.pressure <- data.frame(pressure.db=seq(from=0,to=max(avgtemp.means$pressure.db),by=1))
  avgtemp.dummy <- merge(dummy.pressure,avgtemp.means,all.x=TRUE,by="pressure.db")
  # Fill in the missing values that the merge created with a linear approximation and add a column for the day of the year
  avgfinal <-data.frame(
    approx(avgtemp.dummy$pressure.db, avgtemp.dummy$variable, xout=avgtemp.dummy$pressure.db, method = "linear")[1],
    approx(avgtemp.dummy$pressure.db, avgtemp.dummy$variable, xout=avgtemp.dummy$pressure.db, method = "linear")[2], 
    "month"=daynumber)
  names(avgfinal) <- c("pressure.db","variable","month")
  # The linear approximation can leave NAs at the start of the data frame. Fill with the first whole value.
  avgfinal$variable[is.na(avgfinal$variable)] <- avgfinal$variable[max(which(is.na(avgfinal$variable)))+1]
  names(avgfinal) <- c("pressure.db",column,"month")
  return(avgfinal)
}

temp.temp <- averagingFunc("temp.c")
temp.turb <- averagingFunc("turb.ntu")
temp.ox <- averagingFunc("oxygen.mg")
  
