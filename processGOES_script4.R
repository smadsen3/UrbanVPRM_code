## IAN SMITH
## iasmith [at] bu.edu

# This script extracts hourly surface solar irradiance data for the study domain from the Geostationary Operational Environmental Satellite 16 (GOES16; EUMETSAT OSI SAF)
# This script creates the file rap_goes_NIST30_2018_hourly.rds used in Winbourne et al. 2021
# Directories in this script correspond to the structure of the computing cluster where model calculations were executed.
# To run this code, file paths and directories will need to be restructured to import/write files
# Hourly GOES data were downloaded from ftp://eftp.ifremer.fr/cersat-rt/project/osi-saf/data/radflux/

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

setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/')

# define study domain, city and year
xmin = -77.241397
xmax = -77.202314
ymin =  39.115549
ymax =  39.139102
city = 'NIST30'
yr = 2018

# Set input and create output files directories
inDIR <- paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GOES/2018/origTIFF/')
outDIR <- paste0(city,'/',yr)

# Time file
times <- fread(paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/RAP/2018/times',yr,'.csv')) # times data in /urbanVPRM_30m/driver_data/times/
setkey(times,chr)

# CRS list
LANDSAT_CRS = "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
RAP_CRS = "+proj=lcc +lat_1=25 +lat_2=25 +lat_0=25 +lon_0=265 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"
GOES_CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "

# Import raster of study domain and convert to SpatialPoints object for resampling
ls <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/NIST30/landsat/landsat8/ls0113_8.tif') # landsat data in /urbanVPRM_30m/driver_data/landsat/
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
rl <- list.files(path=inDIR,pattern='GOES') # GOES data downloaded from ftp://eftp.ifremer.fr/cersat-rt/project/osi-saf/data/radflux/

# function to extract GOES data for study domain
goes2landsat <- function(dir,file){
  if(file.exists(paste0(dir,file))){
    print(paste0("Processing file ",file))
    m <- copy(ls)  
    rs <- raster(paste0(dir,file), varname = 'ssi')
    goes.crop <- crop(rs,extent(GOES.XY))
    goes.proj <- projectRaster(goes.crop,crs=LANDSAT_CRS)
    vals <- extract(goes.proj,ls.spdf)
    values(m) <- vals
    y <- list(m)
    return(y)
  }
}

print("done! 2")

# run function
outlist <- mcmapply(goes2landsat, dir=inDIR, file=rl, mc.cores=1)  
rm(ls,ls.spdf,GOES.XY)

# Compile all cropped and reprojected hourly rasters into single "long" data.table
cnames <- substr(rl,1,10)
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
rap2 <- readRDS(paste0(outDIR,'/rap_',city,'_',yr,'_fill.rds'))
setkey(rap2,x,y,datetime)
rap2 <- dm[rap2]
td <- times[,.(datetime,hour)]
td$datetime <- as.character(td$datetime)
setkey(td,datetime)
setkey(rap2,datetime)
rap2 <- td[rap2]

# Deal with GOES NA values
centX <- mean(xmin,xmax)
centY <- mean(ymin,ymax)
# 
sun.rise <- function(x){
  y <- sunrise.set(centY,centX,paste(substr(x,1,4),substr(x,5,6),substr(x,7,8),sep='/'),timezone='UTC')[1]
  return(y)
}
sun.set <- function(x){
  y <- sunrise.set(centY,centX,paste(substr(x,1,4),substr(x,5,6),substr(x,7,8),sep='/'),timezone='UTC')[2]
  return(y)
}
# 
sunrise <- melt.data.table(as.data.table(lapply(times$datetime,sun.rise)),variable.name = 'chr',value.name='posTime')
sunrise[,chr:=seq(length(unique(times$chr)))][,riseTime:=as.numeric(substr(as.character(ymd_hms(posTime)),12,13))]
sunrise <- sunrise[,.(chr,riseTime)]
setkey(sunrise,chr)
sunset <- melt.data.table(as.data.table(lapply(times$datetime,sun.set)),variable.name = 'chr',value.name='posTime')
sunset[,chr:=seq(length(unique(times$chr)))][,setTime:=as.numeric(substr(as.character(ymd_hms(posTime)),12,13))]
sunset <- sunset[,.(chr,setTime)]
setkey(sunset,chr)

rap2[,chr := .GRP, by = .(datetime)]
setkey(rap2,chr)
rap2 <- sunrise[rap2]
rap2 <- sunset[rap2]
invisible(gc())
rap2[is.na(swrad) & hour<=riseTime+1,swrad:=0]
rap2[is.na(swrad) & hour>=setTime-1,swrad:=0]
setorder(rap2,y,x,chr)
len <- dim(rap2[chr==1])[1]
rap2[,Index := .GRP, by = .(x,y)]
rap2 <- rap2[,.(Index,x,y,datetime,chr,tempK,swrad)]
rap2[,tempK:=round(tempK,2)][,swrad:=round(swrad,2)]
setnames(rap2,'chr','HOY')

## Convert Temperature to °C
rap2$tempK = rap2$tempK-273.15
colnames(rap2) = c("Index","x","y","datetime","HoY","tmpC","swRad")

# Assign Index values that correspond to all other driver data
rap2 <- rap2[,-1]

# import raster used for indexing
ls <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/NIST30/landsat/landsat8/ls0113_8.tif') # landsat data in /urbanVPRM_30m/driver_data/landsat/

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
rap2 <- LC.dt[rap2,on = c('x','y'), roll = 'nearest']
setkey(LC.dt, x,y)
setkey(rap2,x,y)

# Save in RDS binary format to preserve space
saveRDS(rap2,paste0(outDIR,'/rap_goes_',city,'_',yr,'_hourly.rds'))
