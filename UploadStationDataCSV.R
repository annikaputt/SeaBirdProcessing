############################
# UploadStationDataCSV.R
# SeaBirdProcessing.Rproj

# Raw Seabird files are uploaded and modified in SeaBirdUpload, then written as csv to
# StationDataCSV. The upload code takes a long time due to the data volume, so 
# this code allows the modified files to be quickly sourced for plotting

# Created November 16, 2015
# A Putt
#############################

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
  tempname <- substr(file_path[i],start=1,stop=7)
  assign(tempname,data.frame(getdata),envir=.GlobalEnv)  
}
