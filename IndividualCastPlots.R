############################
# IndividualCastPlots.R
# SeaBirdProcessing.Rproj

# Some simple plotting to compliment the heatmaps that shows the data from one cast
# This is a good way to make sure that the data make sense from the upload and also look for 
# outliers.

# Created November 16, 2015
# A Putt
#############################

# Read in the data created in SeaBirdUpload.R
source("UploadStationDataCSV.R")


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
