##############################################################################
#### This script contains an example of converting a NetCDF file directly ####
#### downloaded from ECMWF to a dataframe and saving it as a csv file.    ####
#### Created by Elham Nourani...E-mail address: mahle68@gmail.com         ####
#### Laboratory of Animal Ecology, Nagasaki University, Japan...June 2016 ####  
##############################################################################

#Open required libraries

library(RNetCDF)   #for opening and reading NetCDF files and extracting variables and attributes from them
library(lubridate) #for manipulating time variables
library(PCICt)     #for manipulating time variables

#set working directory

setwd("ECMWF-example")

#open the NetCDF file

data<-open.nc("_grib2netcdf-atls06-95e2cf679cd58ee9b4db4dd119a05a8d-8F6WNQ.nc") #this file contains 6-hourly boundary layer height (blh)  
                                                                                #data for October 2013
#print a summary of the file, its variables and attributes
print.nc(data)

#####extract latitude and longitude variables

lon<-var.get.nc(data,"longitude")  #extract longitude
nlon<-dim(lon)                     #extract longitude dimensions

lat<-var.get.nc(data,"latitude")   #extract latitude
nlat<-dim(lat)                     #extract latitude dimensions


#####extract time variable and convert to regular date/time

t <- var.get.nc(data, "time")                     #extract time variable
tunits <- att.get.nc(data, "time", "units")       #extract time units
nt<-dim(t)                                        #extract time dimensions
calendar<-att.get.nc(data, "time", "calendar")    #extract calendar type
tustr<-strsplit(tunits," ")                       #split the unit attribute to extract the origin of the time variable
origin<-unlist(tustr)[3]                          #extract the origin of the time variable
seconds.per.hour <- 3600                          #number of seconds per hour. this is needed to convert the time variable
origin.pcict <- as.PCICt(origin, calendar)        #convert the origin to the regular format

date_time <- origin.pcict + (t * seconds.per.hour)#convert time variable to regular date/time format
date_time<-as.POSIXlt(date_time, tz="UTC")        #convert to POSIX format for easier manipulation; timezone= UTC


####preparing for conversion to a dataframe

row_names<-expand.grid(lon,lat,date_time)    #create a dataframe with three columns: longitude, latitude, time
                                             #This can take up a lot of memory if your NetCDF file is large.

#extract attributes of the variable of interest

vname<-"blh"                                    #set variable name; blh (boundary layer height)
var.array<-var.get.nc(data,vname,unpack=TRUE)   #extract boundary layer height variable
                                                #in files downloaded from ECMWF, variable values are packed to save space, 
                                                #set unpack to TRUE to get the real values


fillvalue<-att.get.nc(data,vname,"_FillValue") #extract the value corresponding to missing values

var.array[var.array==fillvalue]<-NA            #replace missing values with NAs

var.array.long<-as.vector(var.array)           #convert the array into a vector

####create the dataframe

var.mat <- matrix(var.array.long, nrow = nlon * nlat * nt, ncol = 1) #create a matrix with number of rows equal to
                                                                     #number of lat * number of lon * number of time-steps
                                                                     #and with one column for blh values 
var.df<-data.frame(cbind(row_names,var.mat))                         #convert the matrix into a dataframe and bind it to the
                                                                     #row_names dataframe that was created earlier

colnames(var.df)<-c("lon","lat","date_time",vname)                   #set column names

#at this stage, you can use subset() to subset the data spatially and/or temporally before saving 
#the dataframe as a csv file, as it can be very large without subsetting.
#For example, here we will subset the data spatially for an area over Japan & East China Sea:

var.jp<-subset(var.df,lon<=133 & lon>=118 & lat>=26 & lat<=42)

#Also, we will subset temporally to retain only the data for October 24th, 25th and 26th:

var.jp$day<-day(var.jp$date_time) # first, add a column for days in the data frame. 
                                  #if you wish to subset based on hour, week, month, year, etc., 
                                  #use the corresponding functions in lubridate package.

Oct.subset<-subset(var.jp,var.jp$day %in% c(24,25,26)) #subset for only 3 days

####save as csv file

days<-range(Oct.subset$day)     #create a vector of the range of days included in Oct.subset. This will be 
                                #used to name the csv file

write.csv(Oct.subset,paste("ECMWF",vname,days[1],days[2],"Oct.csv",sep="_")) #save Oct.subset in the working
                                                                             # directory with this name:
                                                                             # "ECMWF_blh_24_26_Oct.csv"

