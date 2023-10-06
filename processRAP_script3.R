## IAN SMITH
## iasmith [at] bu.edu

# This script extracts hourly air temperature data for the study domain from the Rapid Refresh Analysis Product (RAP; NOAA)
# This script creates the file rap_NIST30_2018.rds used in Winbourne et al. 2021
# Directories in this script correspond to the structure of the computing cluster where model calculations were executed.
# To run this code, file paths and directories will need to be restructured to import/write files
# Hourly RAP data were downloaded from https://www.ncei.noaa.gov/data/rapid-refresh/access/historical/analysis/

# import packages
library(StreamMetabolism)
library(rgdal)
library(zoo)
library(purrr)
library(data.table)
library(ncdf4)
library(raster)
library(sp)
library(parallel)

setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/')

# define study domain, city and year
#xmin = -79.9333-4/240
#xmax = -79.9333+4/240
#ymin = 44.3167-4/240
#ymax = 44.3167+4/240
#city = 'Borden_V061_500m_2018'

#xmin = -80.3574-4/240
#xmax = -80.3574+4/240
#ymin =  42.7102-4/240
#ymax =  42.7102+4/240
#city = "TP39_V061_500m_2018"

xmin = -80.5577-4/240
xmax = -80.5577+4/240
ymin =  42.6353-4/240
ymax =  42.6353+4/240
city = "TPD_V061_500m_2019"

#xmin = -79.7
#xmax = -79.1
#ymin =  43.5
#ymax =  43.9
#city = 'GTA_V061_500m_2018'

yr = 2018
 

# Set/Create file directories
inDIR <- paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/RAP/2018/origTIFF/')
dir.create(paste0(city),showWarnings = FALSE)
dir.create(paste0(city,'/',yr),showWarnings = FALSE)
outDIR <- paste0(city,'/',yr)
rapDIR <- paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/RAP/2018/RAPgrib/subfolder/')

eraDIR<- paste0('C:/Users/kitty/Documents/Research/SIF/SMUrF/data/ERA5/2018/easternCONUS/')
  
# Time file
times <- fread(paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/RAP/2018/times',yr,'.csv')) # time data found in /urbanVPRM_30m/driver_data/times/
setkey(times,chr)

# CRS list
LANDSAT_CRS = "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
RAP_CRS = "+proj=lcc +lat_1=25 +lat_2=25 +lat_0=25 +lon_0=265 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"
MODIS_CRS = "+proj=longlat +datum=WGS84 +no_defs"
###############################################################################

# Import raster of study domain and convert to SpatialPoints object for resampling
#ls <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD/landsat/landsat8/ls_TPD2018_0203_8_2km_all_bands.tif') # landsat data in /urbanVPRM_30m/driver_data/landsat/
md <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD_V061_500m_2018/LandCover/MODIS_V061_LC_TPD_500m_2018.tif')
values(md) <- 1
#values(ls) <- 1
md.spdf <- as(md, 'SpatialPointsDataFrame')
#ls.spdf <- as(ls,'SpatialPointsDataFrame')

# Create slightly larger bounding box to crop to (avoids dropped pixels at edge when reprojecting / resampling)
bbox <- extent(xmin,xmax,ymin,ymax)
buff=0.05
bbox.extra <- extent(xmin - buff, xmax + buff, ymin - buff, ymax + buff)


# Create extended bounding box raster in Landsat projection 
gridXY = as(raster::extent(bbox.extra), "SpatialPolygons")
proj4string(gridXY) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
dom.bbextra <- as(gridXY,'SpatialPolygonsDataFrame')
RAP.XY <- projectRaster(raster(gridXY),crs=MODIS_CRS)
#RAP.XY<-raster(gridXY,crs=crs(md),res=res(md))
#res(RAP.XY)<-res(md)

# Obtain correct extent for RAP files from one of the raw RAP grib2 data files
rl.grb <- list.files(path=rapDIR,recursive='TRUE',pattern='.grb2') # rap data downloaded from https://www.ncei.noaa.gov/data/rapid-refresh/access/historical/analysis/
RAP_EXT <- extent(raster(paste0(rapDIR,rl.grb[1]))) #DOESN'T UNDERSTAND PROJECTION...?
#looks like I only need to keep one .grb2 file, the rest I can delete I think

# Create file list of converted RAP .tif data files to crop and project 
rl <- list.files(path=inDIR,pattern='rap') #need to create tif files for rap data

## function to extract RAP data for study domain
#rap2landsat <- function(dir,file,domain){
#  print(paste0("Processing file ",file))
#  m <- copy(ls)
#  rs <- raster(paste0(dir,file))
#  extent(rs) = RAP_EXT
#  rs <- projectRaster(rs, crs = crs(ls))
#  rap.crop <- crop(rs,extent(RAP.XY))
#  vals <- extract(rap.crop,ls.spdf)
#  values(m) <- vals
#  y <- list(m)
#  return(y)
#}

# function to extract RAP data for study domain
rap2modis <- function(dir,file,domain){
  print(paste0("Processing file ",file))
  m <- copy(md)
  rs <- raster(paste0(dir,file))
  extent(rs) = RAP_EXT
  rs <- projectRaster(rs, RAP.XY)
  rap.crop <- crop(rs,extent(RAP.XY))
  vals <- extract(rap.crop,md.spdf)
  values(m) <- vals
  y <- list(m)
  return(y)
}

# run function
outlist <- mcmapply(rap2modis, dir=inDIR, file=rl, domain=city, mc.cores=1)
xyvals<-as.data.table(as.data.frame(md.spdf))
colnames(xyvals)[1]<-"tempK"
xyvals$tempK=xyvals$tempK*NA

#rm(md,md.spdf,RAP.XY,RAP_EXT)

# Compile all cropped and reprojected hourly rasters into single "long" data.table
cnames <- paste0(substr(rl,9,16), substr(rl,18,19))
st <- stack(outlist)
dt <- as.data.table(as.data.frame(st,xy=T))
setnames(dt,c('x','y',cnames))

dm <- melt.data.table(dt,id.vars=c('x','y'),variable.name='datetime',value.name='tempK',variable.factor=F)
cat("\n done 1!")
dm <- dm[!is.na(tempK)]
print("done 2!")
setkey(dm,datetime)
td <- times[,.(chr,datetime)]
td[,datetime:=as.character(datetime)]
setkey(td,chr)

#CODE BELOW IS ONLY FOR MISSING DAYS OF RAP DATA:
#For 2019 there are some missing dates
missing_dates<-setdiff(as.numeric(td$datetime),as.numeric(dm$datetime))
#test_dm<-dm[td,on='datetime']
test_dm<-dm

if (length(missing_dates)>0){
  for (d in missing_dates){
    temp_dm<-xyvals
    temp_dm$datetime<-d
    temp_dm<-temp_dm[,c('x','y','datetime','tempK')]
    test_dm<-rbindlist(list(test_dm,temp_dm))
    #print(d)
  }
}
setkey(test_dm,datetime)

test_dm <- td[test_dm, on='datetime']

#If data not missing run this line:
#dm <- td[dm, on = 'datetime']


#For all the missing data replace it with ERA5 temperature

# Create file list of ERA5 .nc data files to crop and project 
eral <- list(list.files(path=eraDIR,pattern='era5_2T')) #need to create tif files for rap data

# function to extract ERA5 data for study domain
era2modis <- function(dir,file,datetime,domain){
  
  yr<-substr(datetime,1,4)
  mon<-substr(datetime,5,6)
  da <- substr(datetime,7,8)
  hr <- substr(datetime,9,10)
  print(paste0("Processing ",datetime))
  ERAstr<-paste0('X',yr,'.',mon,'.',da,'.',hr,'.00.00')
  fl<-file[grep(paste0(yr,mon,'.nc'),file)]
  ERArs <- raster::stack(paste0(dir,fl))
  #ERAnm <- names(ERArs)
  ERArs <- raster::subset(ERArs, ERAstr)
  #rs <- raster(paste0(dir,file))
  #extent(rs) = RAP_EXT
  m <- copy(md)
  ERArs <- projectRaster(ERArs, RAP.XY)
  era.crop <- crop(ERArs,extent(RAP.XY))
  vals <- extract(era.crop,md.spdf)
  values(m) <- vals
  y <- list(m)
  return(y)
}

ERAoutlist <- mcmapply(era2modis, dir=eraDIR, file=eral, datetime=missing_dates, domain=city, mc.cores=1)
cnames <- missing_dates
st <- stack(ERAoutlist)
dt <- as.data.table(as.data.frame(st,xy=T))
setnames(dt,c('x','y',cnames))

ERAdm <- melt.data.table(dt,id.vars=c('x','y'),variable.name='datetime',value.name='tempK',variable.factor=F)
cat("\n done 1!")
ERAdm <- ERAdm[!is.na(tempK)]
print("done 2!")
setkey(ERAdm,datetime)
ERAtd <- times[,.(chr,datetime)]
ERAtd[,datetime:=as.character(datetime)]
setkey(ERAtd,chr)

ERAdm <- ERAtd[ERAdm, on='datetime']

#Test to make sure all the missing values are showing up using this method
#date_vals<-NULL
#for (i in missing_dates){
#  if (length(date_vals)==0){
#    date_vals<-sum(test_dm$tempK[test_dm$datetime==i],na.rm = TRUE)
#    #print(i)
#  }else{
#    date_vals<-append(date_vals,sum(test_dm$tempK[test_dm$datetime==i],na.rm = TRUE))
#    #print(i)
#  }
#}
#date_vals
#test2_dm<-test_dm

#replace missing rap data with ERA5 data
for (i in missing_dates){
  test_dm$tempK[test_dm$datetime==i]<-ERAdm$tempK[ERAdm$datetime==i]
}

#days<-test_dm$datetime[test2_dm$tempK!=test_dm$tempK]

#test2_dm$tempK-ERAdm$tempK


#END OF MISSING DATA FIX



dm <- td[dm, on = 'datetime']

# Save in RDS binary format to preserve space
saveRDS(dm, paste0(outDIR,'/rap_',city,'.rds'))
cat("\n Done 3!")
