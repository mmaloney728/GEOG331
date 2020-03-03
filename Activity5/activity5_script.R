#load in lubridate
library(lubridate)
#read in streamflow data
datH <- read.csv("y:\\Data\\activities\\a05\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)     
#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("y:\\Data\\activities\\a05\\2049867.csv")                          
head(datP)
#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + ((datD$decDay-1)/366),
                       datD$year + ((datD$decDay-1)/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + ((datP$decDay-1)/366),
                       datP$year + ((datP$decDay-1)/365))        

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#QUESTION 3
#number of observations
# streamflow:
length(datD$time)
# there are 393798 streamflow observations.
# precipitation:
length(datP$doy)
# there are 16150 precipitation observations. 

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")
#start new plot
dev.new(width=8,height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border

#IMPROVED LEGEND
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#QUESTION 5
#add a line that shows the observations for 2017 onto this graph of the average
# you may have to adjust the axes limits 
# make the 2017 line a different color than the current colors

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
lines(datD$decDay[which(datD$year==2017)], datD$discharge[which(datD$year==2017)],
      type = "l",
      col="red")
# change the x axis label so that they show each month instead of doy
monthvec <- c(1,32,60,91,121,152,182,213,244,274,305,335,365)
name <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"," ")
axis(1, monthvec, #tick intervals
     lab=name) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation","2017 Observations"), #legend items
       lwd=c(3,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "red"),#colors
       pch=c(NA,15, NA),#symbols
       bty="n")#no legend border



#QUESTION 7
#create a dataframe that indicates what days have a full 24 hours of precip measurements
#grouping by number of observations each day
obsframe <- aggregate(datP, by=list(datP$doy, datP$year), FUN = length)
#pulling out value with all 24 measurements
full.meas <- data.frame(doy=obsframe$Group.1[which(obsframe$doy==24)], 
                        year=obsframe$Group.2[which(obsframe$doy==24)])
 
full.meas$decYear <- ifelse(leap_year(full.meas$year), full.meas$year + ((full.meas$doy-1)/366),
                            full.meas$year + ((full.meas$doy-1)/365))
#plot
par(mai=c(1,1,1,1))
plot(datD$decYear, datD$discharge,
     type = "l",
     xlab = "Date",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)
points(full.meas$decYear, rep(-5, length(full.meas$doy)), pch=20,
       col="blue")
legend("topright", c("Discharge Measurement", "24 Hour Measurements"),
       lwd = c(3, NA),
       pch=c(NA, 20),
       col=c("black", "blue"),
       bty="n")




#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]
min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#QUESTION 8

#January 13 and 14, 2012: include full measurements and precipitation
Q8hydroD <- datD[datD$doy >=13 & datD$doy <15 & datD$year == 2012,]
Q8hydroP <- datP[datP$doy >=13 & datP$doy <15 & datP$year == 2012,]

#min and max of discharge & precipitation:
Q8yl <- floor(min(Q8hydroD$discharge))-1
Q8yh <- ceiling(max(Q8hydroD$discharge))+1
Q8pl <- 0
Q8pm <- ceiling(max(Q8hydroP$HPCP))+.5
#scale precipitation to fit
Q8hydroP$pscale <- (((Q8yh-Q8yl)/(Q8pm-Q8pl)) * Q8hydroP$HPCP) + Q8yl

par(mai=c(1,1,1,1))
#make plot
plot(Q8hydroD$decDay,
     Q8hydroD$discharge,
     type="l",
     ylim=c(Q8yl,Q8yh),
     lwd=2,
     xlab="Day of year (2012)",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(Q8hydroP)){
  polygon(c(Q8hydroP$decDay[i]-0.017,Q8hydroP$decDay[i]-0.017,
            Q8hydroP$decDay[i]+0.017,Q8hydroP$decDay[i]+0.017),
          c(Q8yl,Q8hydroP$pscale[i],Q8hydroP$pscale[i],Q8yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}
legend("topright", c("Discharge Measurement","Precipitation Measurement"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border



library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_boxplot()
#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_violin()

#QUESTION 9
#make a violin plot by season for 2016 and 2017 separately
#2016 plot 
# find dates that seasons start in order to begin separating by season
spring16start<-datD$decYear[which(datD$date=="3/19/2016")][1]
summer16start<-datD$decYear[which(datD$date=="6/20/2016")][1]
fall16start<-datD$decYear[which(datD$date=="9/22/2016")][1]
winter16start<-datD$decYear[which(datD$date=="12/21/2016")][1]
#separate data by season
dat2016<-datD[which(datD$decYear>=2016 & datD$decYear<2017),]
dat2016$season<-ifelse(dat2016$decYear>=spring16start & dat2016$decYear<summer16start,"Spring",
                       ifelse(dat2016$decYear>=summer16start & dat2016$decYear<fall16start, "Summer",
                              ifelse(dat2016$decYear>=fall16start & dat2016$decYear<winter16start,"Fall",
                                     ifelse(dat2016$decYear>=winter16start & dat2016$decYear<2017,"Late 2016 Winter", "Early 2016 Winter"))))
#set seasons as factors to control order of labels
dat2016$season<-factor(dat2016$season, levels = c("Early 2016 Winter", "Spring", "Summer", "Fall", "Late 2016 Winter"))
#make plot
ggplot(data=dat2016,
       aes(season, discharge))+
  geom_violin(aes(fill=season))+
  scale_fill_manual(values=c("slategray3","green4", "gold", "darkorange2", "slategray3" ))+
  xlab("2016 Season")+
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))+
  ggtitle("2016 Streamflow by Season ")




#2017 plot
# find dates that seasons start in order to begin separating by season
spring17start<-datD$decYear[which(datD$date=="3/20/2017")][1]
summer17start<-datD$decYear[which(datD$date=="6/20/2017")][1]
fall17start<-datD$decYear[which(datD$date=="9/22/2017")][1]
winter17start<-datD$decYear[which(datD$date=="12/21/2017")][1]
#separate data by season
dat2017<-datD[which(datD$decYear>=2017 & datD$decYear<2018),]
dat2017$season<-ifelse(dat2017$decYear>=spring17start & dat2017$decYear<summer17start,"Spring",
                       ifelse(dat2017$decYear>=summer17start & dat2017$decYear<fall17start, "Summer",
                              ifelse(dat2017$decYear>=fall17start & dat2017$decYear<winter17start,"Fall",
                                     ifelse(dat2017$decYear>=winter17start & dat2017$decYear<2018,"Late 2017 Winter", "Early 2017 Winter"))))
#set seasons as factors to control order of labels
dat2017$season<-factor(dat2017$season, levels = c("Early 2017 Winter", "Spring", "Summer", "Fall", "Late 2017 Winter"))
#make plot
ggplot(data=dat2017,
       aes(season, discharge))+
  geom_violin(aes(fill=season))+
  scale_fill_manual(values=c("slategray3","green4", "gold", "darkorange2", "slategray3" ))+
  xlab("2017 Season")+
  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1")))+
  ggtitle("2017 Streamflow by Season ")

