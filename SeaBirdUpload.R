############################
# SeaBirdUpload.R
# SeaBirdProcessing.Rproj

# Uploads raw .cnv files and combines by station
# CNV files need to be separated into bins based on their station number S1, A1, ... 

# Created August 31, 2015
# A Putt
############################

library(segmented)

##########################
# seabirdfunc
##########################

# Create a function to process incoming .cnv files
# Notes: skip=232 indicates to start reading at line 233 (there is a lot of header in the file that we don't want)

seabirdfunc <- function(station,datafile) {
  # 1. Basic upload
  getdata <- read.table(sprintf("Data/%s/%s",station,datafile),skip=232,sep="",header=FALSE,
                        col.names=c("scancount","pressure.db","temp.c","cond.us.cm3","turb.ntu","fluor.mg.m3","oxygen.mg","par","flag"))
  getdaterow <- read.csv(sprintf("Data/%s/%s",station,datafile),skip=87,nrow=1,col.names="starttime")   # Add a column with date by pulling the date row from the .cnv file
  getdate <- substr(x=row.names(getdaterow),start=16,stop=35) # Pull the exact digits we need for the date from the string
  getdata$starttime <- getdate
  getdata$date <- substr(x=getdata$starttime,start=1,stop=11) # Pull only the date; remove time as this might be useful later
  
  # 2. Define the surface, cast, and retrieval portions
  # Add a column that details whether the data are from the surface, cast, and retreival
  p.max <- getdata[which(getdata$pressure.db == max(getdata$pressure.db)), ] # Find max pressure, or end of down cast
  # print(p.max)
  # browser()
  downdata <- getdata[1:row.names(p.max)[1],] # Get rid of any data after p.max  
  # Run a piecewise regression to find the point at which the seabird began lowering
  linear <- lm(scancount~pressure.db,data=downdata) # Run a lm to be put into the segmented function call
  X <- downdata$pressure.db # Define the X variable; for some reason this makes the segmented call run properly
  segmented.mod <- segmented(linear, seg.Z = ~X, psi=2) # Run the segmented model; psi is an estimated breakpoint
  break.point <- segmented.mod$psi[2] # Pull the estimated break point of the two models
  cast <- downdata[which(downdata$pressure.db >= break.point), ] # Keep only the data from a higher pressure than the break point
  surface <- downdata[which(downdata$pressure.db < break.point), ] # Save the surface portion
  cast$portion <- "cast"
  surface$portion <- "surface"  
  # Add the retrieval data
  retrieval <- getdata[(as.numeric(row.names(p.max))[1]+1):nrow(getdata),]
  retrieval$portion <- "retrieval"
  
  # 3. Combine the data back into one frame
  wholecast <- rbind(surface,cast,retrieval)
  return(wholecast)
}

##########################
# runstationsfunc
##########################

# Create a function to loop over the seabirdfunc for each station in the project (represented as sub-folder)
# This will create a large data file with all the data from each station stored in 1 frame (all dates)
# Data must be stored in sub-folders in the Data folder labeled by station

runstationsfunc <- function(stationname) {
  file_path  <- list.files(path=sprintf("Data/%s",stationname), pattern="*.cnv")   # Pull all of the subfolders from the data folder
  file_list  <- list()
     
  for(i in 1:length(file_path)){ # Loop through all files.
    tempdata <- file_path[i]
    file_list[[i]] <- seabirdfunc(datafile=tempdata,station=stationname)
  }
  
  seabird <- do.call("rbind",file_list) # Combine into one large data frame
  rownames(seabird) <- 1:nrow(seabird) # Get rid of the long row names that are automatically created
  seabird$station <- stationname # Create a column to define the station number
  return(seabird)
}

##########################
# Run Through Sub-folders
##########################

# Run all stations through the two nestled functions
# Returns a data frame of all the year's scans, with a few extra columns added containing date, station, and poriton of the cast
StationList <- c("S01","S02","S03","S04","S05","S06","A01","A02","A04","C01","C02","C03","C04","C05","C06","C07","C08","C09","C10")
for (i in 1:length(StationList)) {
  name <- sprintf("%sdata.csv",StationList[i])
  stationdata <- runstationsfunc(StationList[i])
  write.csv(stationdata,sprintf("StationDataCSV/%sdata.csv",StationList[i]),row.names=FALSE)
}
