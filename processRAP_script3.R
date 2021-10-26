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

setwd('/projectnb/buultra/iasmith/VPRM_urban_30m')

# define study domain, city and year
xmin = -77.241397
xmax = -77.202314
ymin =  39.115549
ymax =  39.139102
city = 'NIST30'
yr = 2018


# Set/Create file directories
inDIR <- paste0('/projectnb/buultra/iasmith/RAP/2018/origTIFF/')
dir.create(paste0(city),showWarnings = FALSE)
dir.create(paste0(city,'/',yr),showWarnings = FALSE)
outDIR <- paste0(city,'/',yr)
rapDIR <- paste0('/projectnb/buultra/iasmith/RAP/2018/RAPgrib/')

# Time file
times <- fread(paste0('/projectnb/buultra/iasmith/RAP/2018/times',yr,'.csv')) # time data found in /urbanVPRM_30m/driver_data/times/
setkey(times,chr)

# CRS list
LANDSAT_CRS = "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
RAP_CRS = "+proj=lcc +lat_1=25 +lat_2=25 +lat_0=25 +lon_0=265 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"

###############################################################################

# Import raster of study domain and convert to SpatialPoints object for resampling
ls <- raster('/projectnb/buultra/iasmith/VPRM_urban_30m/NIST30/landsat/landsat8/ls0113_8.tif') # landsat data in /urbanVPRM_30m/driver_data/landsat/
values(ls) <- 1
ls.spdf <- as(ls,'SpatialPointsDataFrame')

# Create slightly larger bounding box to crop to (avoids dropped pixels at edge when reprojecting / resampling)
bbox <- extent(xmin,xmax,ymin,ymax)
buff=0.05
bbox.extra <- extent(xmin - buff, xmax + buff, ymin - buff, ymax + buff)

# Create extended bounding box raster in Landsat projection 
gridXY = as(raster::extent(bbox.extra), "SpatialPolygons")
proj4string(gridXY) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
dom.bbextra <- as(gridXY,'SpatialPolygonsDataFrame')
RAP.XY <- projectRaster(raster(gridXY),crs=LANDSAT_CRS)

# Obtain correct extent for RAP files from one of the raw RAP grib2 data files
rl.grb <- list.files(path=rapDIR,pattern='.grb2') # rap data downloaded from https://www.ncei.noaa.gov/data/rapid-refresh/access/historical/analysis/
RAP_EXT <- extent(raster(paste0(rapDIR,rl.grb[1])))

# Create file list of converted RAP .tif data files to crop and project 
rl <- list.files(path=inDIR,pattern='rap')

# function to extract RAP data for study domain
rap2landsat <- function(dir,file,domain){
  print(paste0("Processing file ",file))
  m <- copy(ls)
  rs <- raster(paste0(dir,file))
  extent(rs) = RAP_EXT
  rs <- projectRaster(rs, crs = crs(ls))
  rap.crop <- crop(rs,extent(RAP.XY))
  vals <- extract(rap.crop,ls.spdf)
  values(m) <- vals
  y <- list(m)
  return(y)
}

# run function
outlist <- mcmapply(rap2landsat, dir=inDIR, file=rl, domain=city, mc.cores=1)
rm(ls,ls.spdf,RAP.XY,RAP_EXT)

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
dm <- td[dm, on = 'datetime']

# Save in RDS binary format to preserve space
saveRDS(dm, paste0(outDIR,'/rap_',city,'_',yr,'.rds'))
cat("\n Done 3!")
