## IAN SMITH
## iasmith [at] bu.edu

# This script processes 30m resolution land cover and impervious surface area data from the National Land Cover Database for the VPRM. 
# This script creates the files /urbanVPRM_30m/driver_data/lc_isa/ISA_NIST.tif and /urbanVPRM_30m/driver_data/ls_isa/LC_NIST.tif used in Winbourne et al. 2021.
# Directories in this script correspond to the structure of the computing cluster where model calculations were executed.
# To run this code, file paths and directories will need to be restructured to import/write files
# Landsat 7 and 8 Collection 1 surface reflectance data were downloaded from Google Earth Engine with cropped rasters available on dataverse in /urbanVPRM_30m/driver_data/landsat 
# NLCD land cover and impervious surface area data can be downloaded from https://www.mrlc.gov/data

# import packages
library("data.table")
library("raster")
library("sp")
library("rgdal")
library("shapefiles")

## Create Directories
setwd("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/")
city = 'NIST30'
print("Create directories for NLCD data")
dir.create(paste0(city),showWarnings = FALSE)
dir.create(paste0(city,"/LandCover"),showWarnings = FALSE)
dir.create(paste0(city,"/ISA"),showWarnings = FALSE)

## Define CRS of LANDSAT and NLCD data
LANDSAT_CRS = "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
NLCD_CRS = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80
              +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 

## Import a LANDSAT file cropped to study domain to define grid and extent of the study area.
# Project the grid into NLCD projection to subset NLCD data.
LANDSAT_area =  raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/NIST30/landsat/landsat8/ls0113_8.tif') # landsat data in /urbanVPRM_30m/driver_data/landsat/
npixel = ncell(LANDSAT_area)
print(paste0("number of pixels = ",npixel))
grid_NLCD = projectRaster(LANDSAT_area,crs=NLCD_CRS)  

## Import bounding box defining study domain
bound_box = readOGR(dsn="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/NIST30/shapefiles",layer='NIST_30m_BB') # shapefile data in /urbanVPRM_30m/shapefiles/
bound_box = spTransform(bound_box, LANDSAT_CRS)

## Import, reproject, and crop LC and ISA data
# Land Cover
LC_USA = raster("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/NLCD/LC/nlcd_2019_land_cover_l48_20210604.img") # Land Cover data from https://www.mrlc.gov/data
LC_USAcrop = crop(LC_USA,grid_NLCD)
extent(LC_USAcrop) = extent(grid_NLCD)
LClandsat = projectRaster(LC_USAcrop,crs=LANDSAT_CRS)
LClandsatcrop = crop(LClandsat,LANDSAT_area)
LClandsatcrop = mask(LClandsatcrop,bound_box)

# ISA
ISA_USA = raster("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/NLCD/ISA/nlcd_2019_impervious_l48_20210604.img") # Impervious data from https://www.mrlc.gov/data

ISA_USAcrop = crop(ISA_USA,grid_NLCD)
extent(LC_USAcrop) = extent(grid_NLCD)
ISAlandsat = projectRaster(ISA_USAcrop,crs=LANDSAT_CRS,method="ngb")
ISAlandsatcrop = crop(ISAlandsat,LANDSAT_area)
ISAlandsatcrop = mask(ISAlandsatcrop,bound_box)

## resample for alignment compatibility with other driver datasets
# Land Cover
lc_resample <- resample(LClandsatcrop, LANDSAT_area, method = 'ngb')
# ISA
isa_resample <- resample(ISAlandsatcrop, LANDSAT_area, method = 'ngb')

## Write Rasters
writeRaster(lc_resample,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/NIST30/Landcover/LC_NIST.tif",
            overwrite=TRUE)
writeRaster(isa_resample,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/NIST30/ISA/ISA_NIST.tif",
            overwrite=TRUE)