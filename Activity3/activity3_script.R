#create a function. The names of the arguements for your function will be in parentheses.
#Everything in curly brackets will be run each time the function is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
datW <- read.csv("y:\\Students\\hkropp\\a03\\bewkes_weather.csv",
                 na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data 
print(datW[1,])

#get sensor info from file
# this data table will contain all relevent units
sensorInfo <-   read.csv("y:\\Students\\hkropp\\a03\\bewkes_weather.csv",
                         na.strings=c("#N/A"), nrows=2)

print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])


#use install.packages to install lubridate
#install.packages(c("lubridate"))
#it is helpful to comment this line after you run this line of code on the computer
#and the package installs. You really don't want to do this over and over again.
library(lubridate)

#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))
#a lot of missing values from soil sensor

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#I'm going to make a new column to work with that indicates that I am conducting QAQC
#because overwriting values should be done cautiously and can lead to confusing issues.
#It can be particularily confusing when you are just learning R.
#Here I'm using the ifelse function
#the first argument is a logical statement to be evaluated as true or false on a vector
#the second argument is the value that my air.tempQ1 column will be given if the statement
#is true. The last value is the value that will be given to air.tempQ1 if the statement is false.
#In this case it is just given the air temperature value
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data
#and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  
#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]


#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning    
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#QUESTION 5
assert(length(datW$DD) == length(datW$precipitation), "error: unequal length")
assert(length(datW$DD) == length(lightscale), "error: unequal length")
#This shows that the vectors are all the same length

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

#QUESTION 6
#Repeating the code:
#removing suspect measurements
plot(datW$DD, datW$wind.speed, pch=19, type="b", xlab="Day of Year",
     ylab="Wind Speed (m/s)", main = "Wind Speed Original Plot")
quantile(datW$wind.speed)
#since none of these points appear unreasonable, we can begin to filter out values where there is lightning and high rain
datW$wind.speedQ6 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy >0, NA,
                            ifelse(datW$precipitation >5, NA, datW$wind.speed))
#test using assert to verify that his filtered the data as expected:
assert(length(which(is.na(datW$wind.speed)=="TRUE")) == length(which(is.na(datW$wind.speedQ6)=="TRUE")), "error: unequal length")

#plot with both lines and points of windspeed with the new data
plot(datW$DD, datW$wind.speedQ6, pch=19, type="b", xlab="Day of Year",
     ylab="Wind Speed (m/s)", main = "Wind Speed with New Data")

#QUESTION 7
#first we want to get an idea of which days the soil sensor outage occurred so we can figure out which days are leading up to it
min(datW$doy[which(is.na(datW$soil.moisture))])
#now we can use these days to look at soil moisture and temperature leading up to the failure, as well as precipitation that affects soil moisture
#and air temperature, which affects soil temperature
plot(datW$DD[which(datW$doy==c(187,188,189,190,191))], datW$soil.moisture[which(datW$doy==c(187,188,189,190,191))],pch=19,
     type="b", xlab="Day of Year", ylab="Soil Moisture and Precipitation")
points(datW$DD[which(datW$doy==c(187,188,189,190,191))], datW$precipitation[which(datW$doy==c(187,188,189,190,191))],
       col= "light blue", pch=15, type="p")

legend(190, 0.123,legend=c("Soil Moisture", "Precipitation"),
       col=c("black", "light blue"), pch = c(19,15))
plot(datW$DD[which(datW$doy==c(187,188,189,190,191))], datW$air.tempQ2[which(datW$doy==c(187,188,189,190,191))],
     col= "black", pch=15, type="b", ylab="Air and Soil Temperature", xlab = "Day of Year")  
points(datW$DD[which(datW$doy==c(187,188,189,190,191))], datW$soil.temp[which(datW$doy==c(187,188,189,190,191))],
       col= "tomato3", pch=19)
legend(190, 12,legend=c("Air Temperature", "Soil Temperature"),
       col=c("black", "tomato"), pch = c(15,19))

quantile(datW$soil.moisture, na.rm = TRUE)
min(datW$soil.moisture[which(datW$doy==c(187,188,189,190,191))])


#QUESTION 8
#table with avg air temp, wind speed, soil moisture, soil temp, total precip
#accuracies for rounding: 
#air temp accuracy: p/m 0.6 deg C
#wind speed accuracy: the greater of 0.3 m/s or 3% of measurement
#soil moisture accuracy: 0.03 m^3/m^3
#soil temp accuracy: p/m 1 degree C
#precip accuracy: p/m 5% of measurement from 0 to 50 mm/h
Q8dat <- c(round(mean(datW$air.tempQ2, na.rm = TRUE), 1),round(mean(datW$wind.speedQ6, na.rm = TRUE),1),
           round(mean(datW$soil.moisture, na.rm = TRUE),2),
           round(mean(datW$soil.temp, na.rm = TRUE),0), round(sum(datW$precipitation, na.rm = TRUE),0))
#calculating number of observations for each category
#should find the number of observations, subtracting the values that are NA
obs <- c(length(datW$air.tempQ2)-length(which(is.na(datW$air.tempQ2))),
         length(datW$wind.speedQ6)-length(which(is.na(datW$wind.speedQ6))),
         length(datW$soil.moisture)-length(which(is.na(datW$soil.moisture))),
         length(datW$soil.temp)-length(which(is.na(datW$soil.temp))),
         length(datW$precipitation)-length(which(is.na(datW$precipitation))))

#finding time period
#we know that soil moisture and soil temperature have missing values after a certain date, so we need to consider those time periods
#we know from question 7 that the soil sensor stopped working on day 192, so we will consider the time period to be up to day 191
#the other categories lasted the entirety of the data set, so we can figure out the last day by finding the last entry
time <- c(max(datW$doy)-datW$doy[1],max(datW$doy)-datW$doy[1], 191-datW$doy[1],
          191-datW$doy[1], max(datW$doy)-datW$doy[1])

Q8mat <- rbind(Q8dat,obs,time)
Q8mat <- rbind(Q8dat, round(Q8mat[2:3,],0))

colnames(Q8mat) <- c("Average Air Temperature", "Average Wind Speed", "Average Soil Moisture",
                     "Average Soil Temperature", "Total Precipitation")
rownames(Q8mat) <- c("Data", "Number of Observations", "Time Period (Number of Days)")
Q8mat

#QUESTION 9
par(mfrow=c(2,2))
#plot soil moisture
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)", main = "Soil Moisture for All Observations")
#plot air temp
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)", main = "Air Temperature for All Observations")
#plot soil temp
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)", main = "Soil Temperature for All Observations")
#plot precip
plot(datW$DD, datW$precipitation, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)", main = "Precipitation for All Observations")

