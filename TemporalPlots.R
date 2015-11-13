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
# Get the data manipulated in SeabirdUpload.R
##########################

# Option 1:
# Source the upload file 
# Right now the actual files are not retained in the workspace b/c they are generated in a function, so you have to run the 
# upload code and then read the data back in. But this is faster anyway because the upload takes a long time to run.
source("SeaBirdUpload.R") 

# Option 2:
# If the upload has been performed and the most current files are in the StationDataCSV sub-folder you can just read those files in
file_path  <- list.files(path="StationDataCSV/", pattern="*.csv") # Pull all files
for(i in 1:length(file_path)){
  getdata <- read.csv(sprintf("StationDataCSV/%s",file_path[i]),head=TRUE)
  tempname <- substr(file_path[i],start=1,stop=6)
  assign(tempname,data.frame(getdata),envir=.GlobalEnv)  
}


##########################
# Code tidbits
##########################

# Code to plot data from an individual cast
# Creates a new page for each date
# Subset only the cast (get rid of the retrival and the surface portions
# Create plots of temp, oxygen, cond, turb, fluor

OneStationPlotFunc <- function(StationDataFrame) { # StationDataFrame e.g., S1data, A1data, etc
  castonly <- subset(StationDataFrame,portion=="cast")
  for (i in 1:length(levels(castonly$date))) {
    cast <- subset(castonly,date==levels(castonly$date)[i])
    windows()
    par(mfrow=c(2,3),oma=c(1,1,2,1))
    plot(cast$temp.c,cast$pressure.db,type="l",ylim=rev(range(cast$pressure.db)),ylab="Pressure dbar", xlab="Temp C")
    plot(cast$cond.us.cm3,cast$pressure.db,type="l",ylim=rev(range(cast$pressure.db)),ylab="Pressure dbar", xlab="Conductivity")
    plot(cast$turb.ntu,cast$pressure.db,type="l",ylim=rev(range(cast$pressure.db)),ylab="Pressure dbar", xlab="Turbidity NTU")
    plot(cast$fluor.mg.m3,cast$pressure.db,type="l",ylim=rev(range(cast$pressure.db)),ylab="Pressure dbar", xlab="Fluorescence mg/m3")
    plot(cast$oxygen.mg,cast$pressure.db,type="l",ylim=rev(range(cast$pressure.db)),ylab="Pressure dbar", xlab="Oxygen mg/L")
    mtext(sprintf("%s: %s",cast$station[1],cast$date[1]), side = 3, line=0, outer = TRUE)
  }
}
  
# Run example
# Will automatically open new plot windows; one for each date
OneStationPlotFunc(S1data)

############################
# Code to prepare data for heatmaps
# this represents the code in the start of the seton s04 depth oxy time series plot file
# Need to clean, automate, and apply to a bunch of data

# Turn date into a factor 
temp <- S1data #for now...
temp$posixdate <- as.POSIXct(temp$date,format="%b %d %Y")
temp$pressure.round <- round(temp$pressure,1)

# subset out each date !!! this will go into a function
tosubset <- levels(temp$date)
temp <- subset(temp, date==tosubset[1])

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
  
# Bind the data for each "month" together so that it is ready for plotting
