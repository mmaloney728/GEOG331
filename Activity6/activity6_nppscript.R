#install.packages(c("raster","sp","rgdal","rgeos","plyr"))
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)
#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("~/Downloads/mmaloney/a06/GNPglaciers/GNPglaciers_1966.shp")
plot(g1966, col="black", axes=TRUE)
str(g1966)
g1998 <- readOGR("~/Downloads/mmaloney/a06/GNPglaciers/GNPglaciers_1998.shp")
g2005 <- readOGR("~/Downloads/mmaloney/a06/GNPglaciers/GNPglaciers_2005.shp")
g2015 <- readOGR("~/Downloads/mmaloney/a06/GNPglaciers/GNPglaciers_2015.shp")
str(g2015)
#data stores all accompanying info/measurements for each spatial object
head(g2015@data)
#polygons stores the coordinates for drawing the polygons
#coordinates are connect-the-dots
g2015@polygons[[1]]
g1966@proj4string
#QUESTION 1

#map the glaciers, view by name
spplot(g1966, "GLACNAME")
#check glacier names
g1966@data$GLACNAME
g2015@data$GLACNAME
#2015 and 1966 names don't match
#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))


#raster data - grid of cells
#images measured with sensors - red,green,blue light up to atmosphere
#if you put them together it will look like a photograph
#read in rgb imagery from landsat
redL <- raster("~/Downloads/mmaloney/a06/glacier_09_05_14/l08_red.tif")
greenL <- raster("~/Downloads/mmaloney/a06/glacier_09_05_14/l08_green.tif")
blueL <- raster("~/Downloads/mmaloney/a06/glacier_09_05_14/l08_blue.tif")

#check coordinate system
redL@crs

#make a brick that stacks all layers
# stack is faster if working from a local file
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
#stretch adds contrast to image to see variability and color
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
#redL@crs showed us that we can overlap spatial data
plot(g1966, col="tan3", border=NA, add=TRUE) 
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)
#zoom using ext
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#NDVI Data
#set up years to read in
ndviYear <- seq(2003,2016)

#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
    NDVIraster[[i]] <- raster(paste0("~/Downloads/mmaloney/a06/NDVI/NDVI_",ndviYear[i],".tif"))

  
}
#look at first year
str(NDVIraster[[1]])
#get projection
NDVIraster[[1]]@crs
#different than shp data because it deals with gridded values

plot(NDVIraster[[1]])
#1 is highest amt of vegetation. 0 is no vegetation. neg is water & non-veg features (ignore)

#QUESTION 3
#plot 1966 NDVI glacier polygons

#2003 NDVI data side by side
par(mfrow=c(1,2))
plot(NDVIraster[[1]],axes=TRUE)
plot(g1966, axes=TRUE)


#Glacier Retreat
#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#QUESTION 4
#map both max NDVI and glaciers in 2015
#dont show axes labels with x & y values
#2015 glacier polygon should be no fill color and black border
#what are patterns in NDVI and around glaciers

par(mfrow=c(1,1))
plot(NDVIraster[[which(ndviYear==2015)]],axes=FALSE)
plot(g2015p, add=TRUE, border="black", col=NA)


#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")
#plot areas for each glacier
par(mfrow=c(1,1))
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     axes=TRUE,
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
}

#QUESTION 5
#calculate the % change in area between 1966 and 2015
g2015p@data$pchange.area<-gAll$a2015m.sq/gAll$a1966m.sq
#make an spplot of the glaciers in 2015 showing the % change each glacier has experienced
spplot(g2015p,"pchange.area")


diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)
#plot with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black",border=NA,add=TRUE)

#QUESTION 6
#find the glacier with the largest % loss
largestploss<-min(g2015p@data$pchange.area)
g2015p@data$GLACNAME[which(g2015p@data$pchange.area==largestploss)]
1-largestploss
# boulder glacier has the largest % loss, at only 15.28% of its area in 1966, or at 84.72% of its original area

#make a map that best displays the glacial extent for all years for that glacier with the highest % loss
#include background imagery
#you can use the original data for the map to match with the imagery
#add a map title that includes the % loss and glacier name
#the subset function will be helpful here
# coordinates for where glacier is:
gAll[which(gAll$GLACNAME=="Boulder Glacier"),c("X_COORD","Y_COORD")]
par(mai=c(1,1,1,1))
par(col.axis="white", col.lab="white", tck=0)
#zoom in on glacier
plotRGB(rgbL, ext=c(270500,277400,5423830,5430730), stretch="lin", axes=TRUE, main="Boulder Glacier: 84.72% loss")
box(col="white")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)



#extract NDVI values
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
	#get raster values in the difference polygon
	NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
	#calculate the mean of the NDVI values
	meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}
plot(ndviYear, meanDiff, type="b",
xlab="Year",
ylab="Average NDVI (unitless)",
pch=19)

#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
	if(is.na(x[1])){
		NA}else{
			#fit a regression and extract a slope
			lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters		
NDVIfit <- calc(NDVIstack, fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)


#buffer glaciers
glacier500m <- gBuffer(g1966p, #data to buffer 
byid=TRUE, #keeps original shape id
width=500) #width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
NDVIraster[[1]], #raster to match cells and extent
field=glacier500m@data$GLACNAME, #field to convert to raster data
background=0) #background value for missing data
plot(buffRaster)

#rasterize glaciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]],
field=g1966p@data$GLACNAME, background=0)
#subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)


meanChange <- zonal(NDVIfit, #NDVI function to summarize
glacZones, #raster with zones
"mean")#function to apply
head(meanChange)

#QUESTION 9
#Add the mean change in NDVI per year into the 2015 glacier polygons.
#make a map where the mean change in vegetation is color coded within the 2015 glacier polygons
#remove zone zero and include column with data, not zone labels
#add to 2015 data
g2015p@data$meanChange<-meanChange[-1,2]
spplot(g2015p,"meanChange",col="transparent")




#QUESTION 11
# mean NDVI through all years per pixel
NDVImeanpp<-calc(NDVIstack,mean, na.rm=TRUE) 
# mean of each zone (glacier)
totalMean<-zonal(NDVImeanpp,glacZones,"mean")
# remove zone zero
g2015p@data$NDVImean<-totalMean[-1,2]

colorR <- c("#3333FF", "#00CCFF", "#FF3300", "#000033")
g2015p@data$NDVIcol <- ifelse(g2015p@data$NDVImean<0.2, colorR[1],
	ifelse(g2015p@data$NDVImean>=0.2 & g2015p@data$NDVImean<0.4, colorR[2],
	ifelse(g2015p@data$NDVImean>=0.4 & g2015p@data$NDVImean<0.6, colorR[3],
	colorR[4])))

#plot average maximum for all years
plot(NDVImeanpp)
#overlay color-coded means around buffered glaciers
plot(g2015p, add=TRUE, col=paste(g2015p@data$NDVIcol), border=FALSE)
legend("topright", c("0-0.2", "0.2-0.4", "0.4-0.6", "0.6-1"), fill=colorR, 
	title="Glacier NDVI Means", cex=0.75)


