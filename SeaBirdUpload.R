############################
# SeaBirdUpload.R
# SeaBirdProcessing.Rproj

# Uploads raw .cnv files and combines

# Created August 31, 2015
# A Putt
############################


seabirdfunc <- function(datafile) {
  getdata <- read.table(sprintf("Data/%s",datafile),skip=232,sep="",header=FALSE,
                        col.names=c("scancount","pressure.db","temp.c","cond.us.cm3","turb.ntu","fluor.mg.m3","oxygen.mg","par","flag"))
  # Add a column with date
  getdaterow <- read.csv(sprintf("Data/%s",datafile),skip=87,nrow=1,col.names="starttime")
  getdate <- substr(x=row.names(getdaterow),start=16,stop=35)
  getdata$starttime <- getdate
  
  # Add a column that details whether the data are from the surface, cast, and retreival
  p.max <- getdata[which(getdata$pressure.db == max(getdata$pressure.db)), ] # Find max pressure, or end of down cast
  # print(p.max)
  downdata <- getdata[1:row.names(p.max),] # Get rid of any data after p.max
  retrieval <- getdata[row.names(p.max):nrow(getdata),] # retain retreival data
  retrieval$portion <- "retrieval"
  
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
  
  # Combine the data back into one frame
  wholecast <- rbind(surface,cast,retrieval)
  return(wholecast)
}

# Option to pull only one file
#datafile <- "MON10_13Aug2015052.cnv"
#onecast <- seabirdfunc(datafile=datafile)

# Find the seabird files and run the function through them.
file_path  <- list.files(path="Data", pattern="*.cnv") # Pull all files
file_list  <- list()

# Loop through all files. You may get warnings in the p.max call because sometimes the max 
# pressure occurs for more than one recording. R should only pull the first row name, which should be fine.
for(i in 1:length(file_path)){
  tempdata <- file_path[i]
  file_list[[i]] <- seabirdfunc(datafile=tempdata)
}

seabird <- do.call("rbind",file_list) # Combine into one large data frame
rownames(seabird) <- 1:nrow(seabird) # Get rid of the long row names that are automatically created
