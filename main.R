### Team Lopez members: Bart Driessen & Peter Hooiveld
### January 19 2015
### Setting global settings.
library(downloader)
library(raster)
library(ncdf)
source("R/functions.R")

### Step 1: Download data
download(url= "https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip", destfile= "data/MODIS.zip", mode= "wb")
unzip("data/MODIS.zip",exdir="data/MODIS")

### Step 2: list files and examine them.
filelist <- list.files("data/MODIS", full.names=T )
MODIS2014 <- brick(filelist[1])
MODISjan <- MODIS2014[[1]]
MODISaug <- MODIS2014[[8]]

plot(MODISaug ) 

### Step 3: Downloading municipality data
NLmun <- getData('GADM',country='NLD', level=3)
NLmun <- spTransform(NLmun, CRS(proj4string(MODISjan)))

### Step 4: Calculate NDVI values jan and aug
for (i in c(1:length(NLmun@data$NAME_2))){
  cityname <- NLmun@data$NAME_2[i]
  NLmun@data$greenjan[i]<- greennez(cityname, MODISjan)
}
for (i in c(1:length(NLmun@data$NAME_2))){
  cityname <- NLmun@data$NAME_2[i]
  NLmun@data$greenaug[i]<- greennez(cityname, MODISaug)
}
### Step 5: Plot the NDVImunicipality data of jan and aug of the Neteherlands.
january <- NLmun[NLmun@data$greenjan]


### Step 6: calculate mean yearly mean NDVI for the netherlands
#Create mean NDVI
b <- MODIS2014[[1]]
for(i in c(2:12)){
  a <- MODIS2014[[i]]
  sum <- b+a
  mean <- b/12
}
MODISmean <- mean

#Loop to create municipality means
for (i in c(1:length(NLmun@data$NAME_2))){
  cityname <- NLmun@data$NAME_2[i]
  NLmun@data$greenmean[i]<- greennez(cityname, MODISmean)
}

### Step 7: create plots and extract the names of the max NDVI in jan, aug and the mean.
spplot(NLmun[15])#Special for you Bart, make it beautifull 
spplot(NLmun[16])#Special for you Bart, make it beautifull
spplot(NLmun[17])#Special for you Bart, make it beautifull
#name max NDVI aug, jan and mean



