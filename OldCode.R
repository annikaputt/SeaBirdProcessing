

library(oce)
library(gplots)
library(plyr)

### load, trim  and subset temperature data for each station-----------------------------------

#-------------------------S01------------------------------------------------

data2<-read.ctd("MON10_13Aug2015057.cnv")

data2<-read.table("MON10_13Aug2015057.cnv",skip=232,sep="")


plot(data2) # this plots default plots for this package 

str(data2)
summary(data2)

#have a look at data to see where to cut for downcast data only

plotScan(data2)


#Or you can use trimming tool to automatically trim data

downcast2<-ctdTrim(data2)
plotScan(downcast2)    #have a look at where it was trimmed and see if you are happy with where it was trimmed
summary(downcast2)
# was not trimmed very weel so need to specify where to trim
downcast2<-ctdTrim(data2, "range",
                   parameters=list(item="scan", from=350, to=3126))
summary(downcast2)




#the follwing couple lines of code are to smooth out the data "to interpolate the smoothed results to uniformly-spaced pressure values."
#Note: smoothing the data plots pressure to the nearest meter, may not be a fine enough scale.

#downcast2s<-ctdDecimate(downcast2)

#plotProfile(downcast2s, "temperature")

plot(downcast2)

# Once data has been cleaned up for downcast, we can plot each variable independantly using the plotprofile function 

plotProfile(downcast2, "temperature")

plotProfile(downcast2, "oxygen")

plotProfile(downcast2, "conductivity")

plotProfile(downcast2, "turbidity.")  # this plot shows a negative spike of tubidity at 60m or ~-1385NTU's, need to remove that data

plotProfile(downcast2, "fluorescence.")

summary(downcast2)
#### For heat map plotting of oxygen we need to subset out oxygen and pressure data from all other data in the CNV file

depth.oxygen.b<-data.frame(pressure=downcast2@data$pressure, oxygen=round(downcast2@data$oxygen, 2))
depth.oxygen.a<-subset(depth.oxygen.b, oxygen>0)
plot(pressure~oxygen, data=depth.oxygen.a, type="l")


min(depth.oxygen.a)
mean.depth.ox.a<-ddply(depth.oxygen.a, c("pressure"), summarize, mean.turb=mean(oxygen))
plot(pressure~mean.ox, data=mean.depth.ox.a, type="l")

###to export the depth temperature dataframe for site S01---
write.csv(depth.oxygen.a, "Aug S01 depth oxy.csv")



#-------------------------S02-------------------------------------------------
read.ctd("BRGMon6 CTD data Aug-2014-S02.cnv")

data<-read.ctd("BRGMon6 CTD data Aug-2014-S02.cnv")

plot(data) # this plots default plots for this package 

str(data)
summary(data)

#have a look at data to see where to cut for downcast data only

plotScan(data)


#Or you can use trimming tool to automatically trim data

downcast<-ctdTrim(data)
plotScan(downcast)    #have a look at where it was trimmed and see if you are happy with where it was trimmed

plot(downcast)

summary(downcast)

downcast<-ctdTrim(data, "range",
                  parameters=list(item="scan", from=350, to=3100))

plot(downcast)
summary(downcast)


# Once data has been cleaned up for downcast, we can plot each variable independantly using the plotprofile function 

plotProfile(downcast, "temperature")

plotProfile(downcast, "oxygen")

plotProfile(downcast, "conductivity")

plotProfile(downcast, "turbidity.")

plotProfile(downcast, "fluorescence.")

#### For heat map plotting of turbidity we need to subset out turbidity and pressure data

depth.oxy<-data.frame(pressure=downcast@data$pressure, oxygen=round(downcast@data$oxygen, 2))
plot(pressure~oxygen, data=depth.oxy, type="l")

mean.depth.oxy<-ddply(depth.oxy, c("pressure"), summarize, mean.oxy=mean(oxygen))
plot(pressure~mean.oxy, data=mean.depth.oxy, type="l")

###to export the depth temperature dataframe for site S02---
write.csv(depth.oxy, "Aug S02 depth oxy.csv")


##----------------------------------S03------------------------------------------------------------

data3<-read.ctd("BRGMon6 CTD data Aug-2014-S03.cnv")

plot(data3) # this plots default plots for this package 

summary(data3)

#have a look at data to see where to cut for downcast data only

plotScan(data3)


#Or you can use trimming tool to automatically trim data

downcast3<-ctdTrim(data3)
plotScan(downcast3)    #have a look at where it was trimmed and see if you are happy with where it was trimmed
# this cast may need to be manually trimmed as the depth starts at 1.106

summary(downcast3)

downcast3<-ctdTrim(data3, "range",
                   parameters=list(item="scan", from=300, to=3450))


#downcast3<-ctdTrim(data3, "range",
                   #parameters=list(item="scan", from=490, to=3450))



plot(downcast3)
summary(downcast3)

# Once data has been cleaned up for downcast, we can plot each variable independantly using the plotprofile function 

plotProfile(downcast3, "temperature")

plotProfile(downcast3, "oxygen")

plotProfile(downcast3, "conductivity")

plotProfile(downcast3, "turbidity.")

plotProfile(downcast3, "fluorescence.")

summary(downcast3)

#### For heat map plotting of turbidity we need to subset out turbidity and pressure data

depth.oxy.c<-data.frame(pressure=downcast3@data$pressure, oxygen=round(downcast3@data$oxygen, 2))
plot(pressure~oxygen, data=depth.oxy.c, type="l")

mean.depth.oxy.c<-ddply(depth.oxy.c, c("pressure"), summarize, mean.oxy=mean(oxygen))
plot(pressure~mean.oxy, data=mean.depth.oxy.c, type="l")

head(depth.oxy.c)

###to export the depth temperature dataframe for site S03---
write.csv(depth.oxy.c, "Aug S03 depth oxy.csv")


#------------------------------------------S04-----------------------------------------------------

data4<-read.ctd("BRGMon6 CTD data Oct-2014-S04.cnv")

plot(data4) # this plots default plots for this package 

summary(data4)

#have a look at data to see where to cut for downcast data only

plotScan(data4)



#Or you can use trimming tool to automatically trim data

downcast4<-ctdTrim(data4)
plotScan(downcast4)    #have a look at where it was trimmed and see if you are happy with where it was trimmed



downcast4<-ctdTrim(data4)
plotScan(downcast4)    #have a look at where it was trimmed and see if you are happy with where it was trimmed

summary(downcast4)

plot(downcast4)

downcast4<-ctdTrim(data4, "range",
                   parameters=list(item="scan", from=350, to=2200))


# Once data has been cleaned up for downcast, we can plot each variable independantly using the plotprofile function 

plotProfile(downcast4, "temperature")

plotProfile(downcast4, "oxygen")

plotProfile(downcast4, "conductivity")

plotProfile(downcast4, "turbidity.")

plotProfile(downcast4, "fluorescence.")

summary(downcast4)

#### For heat map plotting of turbidity we need to subset out turbidity and pressure data

depth.oxy.d<-data.frame(pressure=downcast4@data$pressure, oxygen=round(downcast4@data$oxygen, 2))
plot(pressure~oxygen, data=depth.oxy.d, type="l")

mean.depth.oxy.d<-ddply(depth.oxy.d, c("pressure"), summarize, mean.oxy=mean(oxygen))
plot(pressure~mean.oxy, data=mean.depth.oxy.d, type="l")

head(depth.oxy.d)

###to export the depth turbidity dataframe for site S04---
write.csv(depth.oxy.d, "Oct S04 depth oxy.csv")

##-------------------------------------------S05--------------------------------------------------------------

data5<-read.ctd("BRGMon6 CTD data Aug-2014-S05.cnv")

plot(data5) # this plots default plots for this package 

summary(data5)

#have a look at data to see where to cut for downcast data only

plotScan(data5)


#Or you can use trimming tool to automatically trim data

downcast5<-ctdTrim(data5)
plotScan(downcast5)    #have a look at where it was trimmed and see if you are happy with where it was trimmed


summary(downcast5)

#data not trimmed properly
downcast5<-ctdTrim(data5, "range",
                   parameters=list(item="scan", from=350, to=2920))

summary(downcast5)

#data not trimmed properly
#downcast5<-ctdTrim(data5, "range",
                   #parameters=list(item="scan", from=350, to=3100))


plot(downcast5)

# Once data has been cleaned up for downcast, we can plot each variable independantly using the plotprofile function 

plotProfile(downcast5, "temperature")

plotProfile(downcast5, "oxygen")

plotProfile(downcast5, "conductivity")

plotProfile(downcast5, "turbidity.")

plotProfile(downcast5, "fluorescence.")

summary(downcast5)

#### For heat map plotting of turbidity we need to subset out turbidity and pressure data

depth.oxy.e<-data.frame(pressure=downcast5@data$pressure, oxygen=round(downcast5@data$oxygen, 2))
plot(pressure~oxygen, data=depth.oxy.e, type="l")

mean.depth.oxy.e<-ddply(depth.oxy.e, c("pressure"), summarize, mean.oxy=mean(oxygen))
plot(pressure~mean.oxy, data=mean.depth.oxy.e, type="l")

head(depth.oxy.e)

###to export the depth turbidity dataframe for site S05---
write.csv(depth.oxy.e, "Aug S05 depth oxy.csv")


#####s06##########################


data1<-read.ctd("BRGMon6 CTD data Aug-2014-S06.cnv")

plot(data1) # this plots default plots for this package 

str(data1)
summary(data1)

#have a look at data to see where to cut for downcast data only

plotScan(data1)


#Or you can use trimming tool to automatically trim data

downcast1<-ctdTrim(data1)
plotScan(downcast1)    #have a look at where it was trimmed and see if you are happy with where it was trimmed

summary(downcast1)

plot(downcast1)

downcast1<-ctdTrim(data1, "range",
                   parameters=list(item="scan", from=300, to=2076))


summary(downcast1)

# Once data has been cleaned up for downcast, we can plot each variable independantly using the plotprofile function 

plotProfile(downcast1, "temperature")

plotProfile(downcast1, "oxygen")

plotProfile(downcast1, "conductivity")

plotProfile(downcast1, "turbidity.")

plotProfile(downcast1, "fluorescence.")

#### For heat map plotting of turbidity we need to subset out turbidity and pressure data

depth.oxy.b<-data.frame(pressure=downcast1@data$pressure, oxygen=round(downcast1@data$oxygen, 2))
plot(pressure~oxygen, data=depth.oxy.b, type="l")

mean.depth.oxy.b<-ddply(depth.oxy.b, c("pressure"), summarize, mean.oxy=mean(oxygen))
plot(pressure~mean.oxy, data=mean.depth.oxy.b, type="l")

###to export the depth turbidity dataframe for site S06---
write.csv(depth.oxy.b, "Aug S06 depth oxy.csv")

