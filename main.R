### Team Lopez members: Bart Driessen & Peter Hooiveld
### January 19 2015
### Setting global settings.
library(downloader)
library(raster)
library(ncdf)
library(rgdal)
library(ggplot2)

source("R/functions.R")

### Step 1: Download data
download(url= "https://github.com/GeoScripting-WUR/VectorRaster/raw/gh-pages/data/MODIS.zip", destfile= "data/MODIS.zip", mode= "wb")
unzip("data/MODIS.zip",exdir="data/MODIS")

### Step 2: List files and examine them.
filelist <- list.files("data/MODIS", full.names=T )
MODIS2014 <- brick(filelist[1])
MODISjan <- MODIS2014[[1]]
MODISaug <- MODIS2014[[8]]
plot(MODISaug ) 

### Step 3: Downloading municipality data
NLmun <- getData('GADM',country='NLD', level=3)
NLmun <- spTransform(NLmun, CRS(proj4string(MODISjan)))

### Step 4: Calculate NDVI values January and August
for (i in 1:length(NLmun@data$NAME_2)){
  cityname <- NLmun@data$NAME_2[i]
  NLmun@data$greenjan[i]<- greennez(cityname, MODISjan)
}
for (i in 1:length(NLmun@data$NAME_2)){
  cityname <- NLmun@data$NAME_2[i]
  NLmun@data$greenaug[i]<- greennez(cityname, MODISaug)
}

### Step 5: Plot the NDVI municipality data of January and August of the Netherlands.
# Spatial plots
spplot(NLmun,zcol='greenjan',main = 'Average NDVI in January')
spplot(NLmun,zcol='greenaug',main = 'Average NDVI in August')

# Normal plots
January <- data.frame("city"=1:length(NLmun@data$NAME_2),"NDVI"=1:length(NLmun@data$NAME_2))
for(i in 1:length(NLmun@data$NAME_2)){
  January$city[i] <- NLmun@data$NAME_2[i]
  January$NDVI[i] <- NLmun@data$greenjan[i]
}

August <- data.frame("city"=1:length(NLmun@data$NAME_2),"NDVI"=1:length(NLmun@data$NAME_2))
for(i in 1:length(NLmun@data$NAME_2)){
  August$city[i] <- NLmun@data$NAME_2[i]
  August$NDVI[i] <- NLmun@data$greenaug[i]
}

# (January)
cityselection <- c('Amersfoort', 'Wageningen', 'Swalmen','Amsterdam','Groenlo')
NDVIselection <- c(1:length(cityselection))
for(i in 1:length(cityselection)){
  for(j in 1:length(January$city)){
    if(January$city[j] == cityselection[i]){
      NDVIselection[i] <- January$NDVI[January$city == cityselection[i]]
    }
  }
}

# (August)
cityselection2 <- c('Amersfoort', 'Wageningen', 'Swalmen','Amsterdam','Groenlo')
NDVIselection2 <- c(1:length(cityselection))
for(i in 1:length(cityselection2)){
  for(j in 1:length(August$city)){
    if(August$city[j] == cityselection2[i]){
      NDVIselection2[i] <- August$NDVI[August$city == cityselection2[i]]
    }
  }
}

qplot(cityselection, NDVIselection,ylab = 'NDVI', xlab = 'Cities',main = 'Mean NDVI for January for a number of cities')
qplot(cityselection2, NDVIselection2,ylab = 'NDVI', xlab = 'Cities',main = 'Mean NDVI for August for a number of cities')

### Step 6: calculate mean yearly mean NDVI for the netherlands
# Calculate mean NDVI
sum <- MODIS2014[[1]]
for(i in 2:12){
  a <- MODIS2014[[i]]
  sum <- sum+a
}
MODISmean <- sum/12

# Adding the mean NDVI to the NLmun dataset at a municipal level
for (i in 1:length(NLmun@data$NAME_2)){
  cityname <- NLmun@data$NAME_2[i]
  NLmun@data$greenmean[i]<- greennez(cityname, MODISmean)
}

### Step 7: create plots and extract the names of the max NDVI in jan, aug and the mean.
spjan <- spplot(NLmun, zcol='greenjan')
spaug <- spplot(NLmun, zcol='greenaug')
print(spjan, split=c(1, 1, 2, 1), more=TRUE,main='The NDVI of January (to the left) and August (to the right)')
print(spaug, split=c(2, 1, 2, 1), more=FALSE) 
spplot(NLmun, zcol='greenmean',main='The average yearly NDVI at a municipal level')

# Deriving the city with the largest NDVI of the Netherlands.
sprintf('The city with the highest NDVI in January is %s and has an NDVI of %f', January$city[January$NDVI == max(January$NDVI)], January$NDVI[January$NDVI == max(January$NDVI)])
sprintf('The city with the highest NDVI in August is %s and has an NDVI of %f', August$city[August$NDVI == max(August$NDVI)], August$NDVI[August$NDVI == max(August$NDVI)])
sprintf('The city with the highest average yearly NDVI is %s and has an NDVI of %f', NLmun$NAME_2[NLmun$greenmean == max(NLmun$greenmean)], NLmun$greenmean[NLmun$greenmean == max(NLmun$greenmean)])

### Bonus: provincial level.
provinces <- NLmun$NAME_1[!duplicated(NLmun$NAME_1)]
provinces <- data.frame(Provinces = provinces, Mean = 1:length(provinces))
for(i in 1:length(provinces$Provinces)){
  province <- as.character(provinces$Provinces[i])
  sum <- 0
  count <- 0
  for (j in 1:length(NLmun@data$NAME_2)){
    if(NLmun@data$NAME_1[j] == province){
      count = count + 1
      sum <- sum + NLmun@data$greenmean[j]
    }
  }
  mean <- sum / count
  provinces$Mean[i] <- mean
}

for(i in 1:length(provinces$Provinces)){
  for (j in 1:length(NLmun@data$NAME_2)){
    if(NLmun@data$NAME_1[j] == as.character(provinces$Provinces[i])){
      NLmun$provmean[j] <- provinces$Mean[i]
    }
  }
}


NLprov <- getData('GADM',country='NLD', level=1)
NLprov <- spTransform(NLprov, CRS(proj4string(MODISjan)))
for(i in 1:length(NLprov)){
  for(j in 1:length(NLprov)){
    if(NLprov$NAME_1[i] == as.character(provinces$Provinces[j])){
      NLprov$provmean[i] <- provinces$Mean[j]
    }
  }
}

# Finally, plotting the provincial mean NDVI.
spplot(NLprov, zcol='provmean', main='Average yearly NDVI at a provincial level')

