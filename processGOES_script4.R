memory.limit(size=5e8)
## IAN SMITH
## iasmith [at] bu.edu

# This script extracts hourly surface solar irradiance data for the study domain
# from the Geostationary Operational Environmental Satellite 16 (GOES16; EUMETSAT OSI SAF)
# This script creates the file rap_goes_NIST30_2018_hourly.rds used in Winbourne et al. 2021
# Directories in this script correspond to the structure of the computing cluster 
# where model calculations were executed.
# To run this code, file paths and directories will need to be restructured to 
# import/write files
# Hourly GOES data were downloaded from 
# ftp://eftp.ifremer.fr/cersat-rt/project/osi-saf/data/radflux/

library(StreamMetabolism)
library(rgdal)
library(zoo)
library(purrr)
library(data.table)
library(ncdf4)
library(raster)
library(sp)
library(parallel)
library(lubridate)

setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files')


#setwd('/projectnb/buultra/iasmith/VPRM_urban_30m')

# define study domain, city and year
xmin = -79.9333-4/240
xmax = -79.9333+4/240
ymin = 44.3167-4/240
ymax = 44.3167+4/240
city = 'Borden_500m_V061_adjusted_R_2019'

#xmin = -80.5577-4/240
#xmax = -80.5577+4/240
#ymin =  42.6353-4/240
#ymax =  42.6353+4/240
#city = 'TPD_500m_V061_adjusted_R_2018'

#xmin = -80.3574-4/240
#xmax = -80.3574+4/240
#ymin =  42.7102-4/240
#ymax =  42.7102+4/240
#city = 'TP39_500m_V061_adjusted_R_2018'

#xmin = -79.7
#xmax = -79.1
#ymin =  43.5
#ymax =  43.9
#city = 'GTA_500m_2019'

yr = 2018

# Set input and create output files directories
#inDIR <- paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GOES/2019/origTIFF/')
outDIR <- paste0(city,'/',yr)

# Time file
times <- fread(paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/RAP/2018/times',yr,'.csv')) # times data in /urbanVPRM_30m/driver_data/times/
setkey(times,chr)

# CRS list
#LANDSAT_CRS = "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
RAP_CRS = "+proj=lcc +lat_1=25 +lat_2=25 +lat_0=25 +lon_0=265 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"
GOES_CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "
MODIS_CRS = "+proj=longlat +datum=WGS84 +no_defs"

# Import raster of study domain and convert to SpatialPoints object for resampling
#ls <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD/landsat/landsat8/ls_TPD2018_0203_8_2km_all_bands.tif') # landsat data in /urbanVPRM_30m/driver_data/landsat/
ls <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden_500m_V061_adjusted_R_2018/LandCover/MODIS_LC_Borden_500m_V061_adjusted_R_2018.tif')
npixel <- ncell(ls)
values(ls) <- 1
ls.spdf <- as(ls,'SpatialPointsDataFrame')

# Create slightly larger bounding box to crop to (avoids dropped pixels at edge when reprojecting / resampling)
bbox <- extent(xmin,xmax,ymin,ymax)
buff=0.05
bbox.extra <- extent(xmin - buff, xmax + buff, ymin - buff, ymax + buff)

# Create extended bounding box raster in GOES projection 
gridXY = as(raster::extent(bbox.extra), "SpatialPolygons")
proj4string(gridXY) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
GOES.XY <- projectRaster(raster(gridXY),crs=GOES_CRS)

print("done! 1")

# Create file list of data to project / crop / resample
#rl <- list.files(path=inDIR,pattern='GOES') # GOES data downloaded from ftp://eftp.ifremer.fr/cersat-rt/project/osi-saf/data/radflux/

# function to extract GOES data for study domain
#goes2landsat <- function(dir,file){
#  if(file.exists(paste0(dir,file))){
#    print(paste0("Processing file ",file))
#    m <- copy(ls)  
#    rs <- raster(paste0(dir,file), varname = 'ssi')
#    goes.crop <- crop(rs,extent(GOES.XY))
#    goes.proj <- projectRaster(goes.crop,crs=LANDSAT_CRS)
#    vals <- extract(goes.proj,ls.spdf)
#    values(m) <- vals
#    y <- list(m)
#    return(y)
#  }
#}

goes_data<-readRDS("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden_500m_V061_adjusted_R_2018/pre_processed_GOES/goes_Borden_2018_pre_processed_mean_filling_NA_rm.rds")
goes_data<-goes_data[,.(x,y,datetime,sw_test)]

outlist<-NULL
for (d in unique(goes_data$datetime)){
  m<-copy(ls)
  #print(paste0("Processing ",d))
  godat<-rasterFromXYZ(goes_data[goes_data$datetime==d])
  godat<-godat$sw_test
  crs(godat)<-GOES_CRS
  #godat<-crop(godat,extent(GOES.XY))
  #godat<-projectRaster(godat,crs=MODIS_CRS)
  vals<- extract(godat,ls.spdf)
  values(m)<-vals
  outlist<-append(outlist,m)
}

#goes2modis <- function(dir,file){
#  if(file.exists(paste0(dir,file))){
#    print(paste0("Processing file ",file))
#    m <- copy(ls)  
#    rs <- raster(paste0(dir,file), varname = 'ssi')
#    rs_c <- raster(paste0(dir,file), varname = 'ssi_confidence_level')
#    rs[rs_c<3]<-NA # Remove values which have a 'bad' confidence level
#    goes.crop <- crop(rs,extent(GOES.XY))
#    goes.proj <- projectRaster(goes.crop,crs=MODIS_CRS)
#    vals <- extract(goes.proj,ls.spdf)
#    values(m) <- vals
#    y <- list(m)
#    return(y)
#  }
#}

print("done! 2")

# run function
#outlist <- mcmapply(goes2modis, dir=inDIR, file=rl, mc.cores=1)  
rm(ls,ls.spdf,GOES.XY)

# Compile all cropped and reprojected hourly rasters into single "long" data.table
cnames <- as.character(times$datetime)#substr(rl,1,10)
st = stack(outlist) 
dt <- as.data.table(as.data.frame(st,xy=T))
setnames(dt,c('x','y',cnames))    
print("saved dt")
dt <- setDT(dt)        
dm <- melt.data.table(dt,id.vars=c('x','y'),variable.name='datetime',value.name='swrad',variable.factor=F)
dm[,datetime:=as.character(datetime)][,swrad:=as.numeric(swrad)]
setkey(dm,x,y,datetime)

# GOES ssi data has NAs where the archive files are not available. These dates must be inspected, since interpolation to fill NAs is inappropriate for nighttime hours. For the year 2018, all of the missing data occur during nighttime hours. These are set to zero for the year.

# Load RAP .rds file to join with GOES completed data.table
rap2 <- readRDS(paste0(outDIR,'/rap_',city,'.rds'))#'_',yr,'.rds'))
setkey(rap2,x,y,datetime)
rap2 <- dm[rap2]
td <- times[,.(datetime,hour)]
td$datetime <- as.character(td$datetime)
setkey(td,datetime)
setkey(rap2,datetime)
rap2 <- td[rap2]

#Already did htis in pre-processing code
## Deal with GOES NA values
#centX <- mean(xmin,xmax)
#centY <- mean(ymin,ymax)
# 
#sun.rise <- function(x){
#  y <- sunrise.set(centY,centX,paste(substr(x,1,4),substr(x,5,6),substr(x,7,8),sep='/'),timezone='UTC')[1]
#  return(y)
#}
#sun.set <- function(x){
#  y <- sunrise.set(centY,centX,paste(substr(x,1,4),substr(x,5,6),substr(x,7,8),sep='/'),timezone='UTC')[2]
#  return(y)
#}
## 
#sunrise <- melt.data.table(as.data.table(lapply(times$datetime,sun.rise)),variable.name = 'chr',value.name='posTime')
#sunrise[,chr:=seq(length(unique(times$chr)))][,riseTime:=as.numeric(substr(as.character(ymd_hms(posTime)),12,13))]
#sunrise <- sunrise[,.(chr,riseTime)]
#setkey(sunrise,chr)
#sunset <- melt.data.table(as.data.table(lapply(times$datetime,sun.set)),variable.name = 'chr',value.name='posTime')
#sunset[,chr:=seq(length(unique(times$chr)))][,setTime:=as.numeric(substr(as.character(ymd_hms(posTime)),12,13))]
#sunset <- sunset[,.(chr,setTime)]
#setkey(sunset,chr)

rap2[,chr := .GRP, by = .(datetime)]
setkey(rap2,chr)
#rap2 <- sunrise[rap2]
#rap2 <- sunset[rap2]
invisible(gc())

#rap2[swrad<=0, swrad:=NA]

#rap2[is.na(swrad) & hour<=riseTime+1,swrad:=0]
#rap2[is.na(swrad) & hour>=setTime-1,swrad:=0]




#rap_test=rap2 #make a copy of the dataframe
##make an array with indices where swrad is missing
#sw_na <- which((is.na(rap_test$swrad)) & (rap_test$hour>rap_test$riseTime+1) & (rap_test$hour<rap_test$setTime-1))

##loop over missing swrad data and fill it with average of data from the hour before and the hour after
#for (i in sw_na){
#  rap_test$swrad[i]<-(rap_test$swrad[i+13824]+rap_test$swrad[i-13824])/2
#}




setorder(rap2,y,x,chr)
len <- dim(rap2[chr==1])[1]
rap2[,Index := .GRP, by = .(x,y)]
rap2 <- rap2[,.(Index,x,y,datetime,chr,tempK,swrad)]
rap2[,tempK:=round(tempK,2)][,swrad:=round(swrad,2)]
setnames(rap2,'chr','HOY')



##There are still some remaining swrad NA values during the day, 
## try to interpolate to get these values
### Set up new dataframe for swrad Interpolation
#npixel <- length(unique(rap2$Index))
#inter_swrad <- as.data.frame(matrix(ncol = 2, nrow = npixel*365*24))
#colnames(inter_swrad) <- c('Index', 'HOY')
#inter_swrad[,1] <- rep(seq(1,npixel,1), 365*24)
#inter_swrad <- inter_swrad[order(inter_swrad[,1]),]
#inter_swrad[,2] <- rep(seq(1,365*24,1), npixel)
#inter_swrad <- merge(inter_swrad, rap2[,c('Index', 'swrad', 'HOY')], by = c('Index', 'HOY'), all = TRUE)
#inter_swrad <- inter_swrad[order(inter_swrad[,'Index'],inter_swrad[,'HOY']),]


## add coordinates
#x <- as.data.frame(tapply(rap2$x, rap2$Index, mean, na.rm = T))
#y <- as.data.frame(tapply(rap2$y, rap2$Index, mean, na.rm = T))
#xy <- cbind(as.numeric(rownames(x)), x, y)
#colnames(xy) <- c('Index', 'x', 'y')

#inter_swrad <- merge(inter_swrad, xy, by = 'Index', all = TRUE)
#inter_swrad <- inter_swrad[,c('Index', 'HOY', 'x', 'y', 'swrad')]

## Interpolate hourly swrad values for each pixel
#inter_swrad$swrad_inter <- NA

#for(i in unique(inter_swrad$Index)){
#  if(is.finite(mean(inter_swrad[which(inter_swrad$Index == i),'swrad'], na.rm = T)) & length(inter_swrad[which(inter_swrad$Index == i & is.finite(inter_swrad$swrad)),'swrad']) > 10){
#    pix <- inter_swrad[which(inter_swrad$Index == i),]
#    spl <- with(pix[!is.na(pix$swrad),],smooth.spline(DOY,swrad, spar = .25)) 
#    inter_swrad[which(inter_swrad$Index == i),'swrad_inter'] <- predict(spl, c(1:365*24))$y
#  } else {inter_swrad[which(inter_swrad$Index == i),'swrad_inter'] <- NA}
#  print(round(i/npixel*100, 1))
#}





## Convert Temperature to °C
rap2$tempK = rap2$tempK-273.15
colnames(rap2) = c("Index","x","y","datetime","HoY","tmpC","swRad")

# Assign Index values that correspond to all other driver data
rap2 <- rap2[,-1]

# import raster used for indexing
#ls <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD/landsat/landsat8/ls_TPD2018_0203_8_2km_all_bands.tif') # landsat data in /urbanVPRM_30m/driver_data/landsat/
ls <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden_500m_V061_adjusted_R_2018/LandCover/MODIS_LC_Borden_500m_V061_adjusted_R_2018.tif')


## Function to convert tif into a datatable..
tifdt_fun = function(raster,name){
  dt = as.data.table(as.data.frame(raster, xy=T))
  dt = cbind(1:ncell(raster), dt)
  setnames(dt,c("Index","x","y",name))
  setkey(dt,Index,x,y)
  return(dt)
}

Idx.dt = tifdt_fun(ls,"Indexing")
Idx.dt <- Idx.dt[,-4]
colnames(Idx.dt) <- c('Index','x','y')

# Add new Index to the rap/goes data table
rap2 <- Idx.dt[rap2,on = c('x','y'), roll = 'nearest']
setkey(Idx.dt, x,y)
setkey(rap2,x,y)

# Save in RDS binary format to preserve space
saveRDS(rap2,paste0(outDIR,'/rap_goes_',city,'_hourly_fixed.rds'))
