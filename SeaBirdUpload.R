############################
# SeaBirdUpload.R
# SeaBirdProcessing.Rproj

# Uploads raw .cnv files and combines
# CNV files need to be separated into bins based on their station number S1, A1, ... 

# Created August 31, 2015
# A Putt
############################

########## seabirdfunc ##########

# Create a function to process incoming .cnv files
# Notes: skip=232 indicates to start reading at line 233 (there is a lot of header in the file)

seabirdfunc <- function(station,datafile) {
  getdata <- read.table(sprintf("Data/%s/%s",station,datafile),skip=232,sep="",header=FALSE,
                        col.names=c("scancount","pressure.db","temp.c","cond.us.cm3","turb.ntu","fluor.mg.m3","oxygen.mg","par","flag"))
  # Add a column with date
  # Pull the row from the header that has the date
  getdaterow <- read.csv(sprintf("Data/%s/%s",station,datafile),skip=87,nrow=1,col.names="starttime")
  getdate <- substr(x=row.names(getdaterow),start=16,stop=35) # Pull the exact digits we need for the date
  getdata$starttime <- getdate
  getdata$date <- substr(x=getdata$starttime,start=1,stop=11)
  
  # Add a column that details whether the data are from the surface, cast, and retreival
  p.max <- getdata[which(getdata$pressure.db == max(getdata$pressure.db)), ] # Find max pressure, or end of down cast
  # print(p.max)
  # browser()
  downdata <- getdata[1:row.names(p.max)[1],] # Get rid of any data after p.max  
  # Run a piecewise regression to find the point at which the seabird began lowering
  library(segmented)
  linear <- lm(scancount~pressure.db,data=downdata) # Run a lm to be put into the segmented call
  X <- downdata$pressure.db # Pull the X variable; for some reason this makes the segmented call run properly
  segmented.mod <- segmented(linear, seg.Z = ~X, psi=2) # psi is an estimated breakpoint
  break.point <- segmented.mod$psi[2] # Pull the estimated break point of the two models
  cast <- downdata[which(downdata$pressure.db >= break.point), ] # Keep only the data from a higher pressure than the break point
  surface <- downdata[which(downdata$pressure.db < break.point), ] # Save the equilibriation
  cast$portion <- "cast"
  surface$portion <- "surface"
  
  # Add the retrieval data
  # browser()
  retrieval <- getdata[(as.numeric(row.names(p.max))[1]+1):nrow(getdata),]
  retrieval$portion <- "retrieval"
  
  # Combine the data back into one frame
  wholecast <- rbind(surface,cast,retrieval)
  return(wholecast)
}

########## runstationsfunc ##########

# Create a function to loop over the seabirdfunc for each station in the project
# This will create a large data file with all the data from each station stored in 1 frame (all dates)
# Data must be stored in sub-folders in the Data folder labeled by station

runstationsfunc <- function(stationname) {
  # Find the seabird files and run the function through them.
  file_path  <- list.files(path=sprintf("Data/%s",stationname), pattern="*.cnv") # Pull all files
  file_list  <- list()
  
  # Loop through all files. 
  for(i in 1:length(file_path)){
    tempdata <- file_path[i]
    file_list[[i]] <- seabirdfunc(datafile=tempdata,station=stationname)
  }
  
  seabird <- do.call("rbind",file_list) # Combine into one large data frame
  rownames(seabird) <- 1:nrow(seabird) # Get rid of the long row names that are automatically created
  seabird$station <- stationname # Create a column to define the station number
  return(seabird)
}

#########################
# Run all stations
# Returns a data frame of all the year's scans, with a few extra columns added containing date, station, and poriton of the cast
# !!!!! I should really streamline this and also add code to write the data frames so that the code doesn't need to be re-run each time.
S1data <- runstationsfunc("S1")
# S2data <- runstationsfunc("S2")
# S3data <- runstationsfunc("S3")
# S4data <- runstationsfunc("S4")
# S5data <- runstationsfunc("S5")
# S6data <- runstationsfunc("S6")
# A1data <- runstationsfunc("A1")
# A2data <- runstationsfunc("A2")
# C1data <- runstationsfunc("C1")
# C2data <- runstationsfunc("C2")
# C3data <- runstationsfunc("C3")
# C4data <- runstationsfunc("C4")
# C5data <- runstationsfunc("C5")
# C6data <- runstationsfunc("C6")
# C7data <- runstationsfunc("C7")
# C8data <- runstationsfunc("C8")
# C9data <- runstationsfunc("C9")
# C10data <- runstationsfunc("C10")
