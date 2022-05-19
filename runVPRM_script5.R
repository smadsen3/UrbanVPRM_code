memory.limit(size=5e8)
## IAN SMITH
## iasmith [at] bu.edu

# This script imports all VPRM driver data, sources VPRM parameters and equations, and executes the model
# This script creates the file vprm_30m_nist.csv used in Winbourne et al. 2021
# Directories in this script correspond to the structure of the computing cluster where model calculations were executed.
# To run this code, file paths and directories will need to be restructured to import/write files

## Load libraries
library("data.table")
library("raster")
library("parallel")
library("doParallel")
library("foreach")
library("ggplot2") #not needed for running just visualizing

## register cores for parallel processes on computing cluster
cores = as.numeric(Sys.getenv("NSLOTS"))
if (is.na(cores)) cores=1
registerDoParallel(cores)
print(paste0("n. of cores is ",cores))

#setwd("/projectnb/buultra/iasmith/VPRM_urban_30m/")

setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files')

# Arguments: 
city = 'TPD_SOLRIS_updated_R'
yr = 2018
veg_type = 'DBF' #Maybe use Mixed forest instead?

## If area is too big (n of pixels > nrow_block) divide in blocks of nrow_block cells
nrow_block=17000

# Climate data folder
dir_clima = paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD/2018') # climate data in /urbanVPRM_30m/driver_data/rap_goes/

## Define the path to the folder where outputs are saved 
dir.create(paste0("outputs"), showWarnings = FALSE)

dir.create(paste0("outputs/",city,"/test"), showWarnings = FALSE)
dir_out = paste0(city,"/test")

## Function to convert tif into a datatable..
tifdt_fun = function(raster,name){
  dt = as.data.table(as.data.frame(raster, xy=T))
  dt = cbind(1:ncell(raster), dt)
  setnames(dt,c("Index","x","y",name))
  setkey(dt,Index,x,y)
  return(dt)
}

### LOAD DATA
## Land cover and ISA
## NEED TO CONVERT LC DATA TO SAME FORMAT AS NLCD DATA
LC = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD_SOLRIS/Landcover/LC_TPD.tif') # Land cover data in /urbanVPRM_30m/driver_data/lc_isa/
LC.dt = tifdt_fun(LC,"LandCover")
#LC_NIST = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/NIST30/Landcover/LC_NIST.tif') # Land cover data in /urbanVPRM_30m/driver_data/lc_isa/
#LC_NIST.dt = tifdt_fun(LC_NIST,"LandCover")

ISA_dat = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD_SOLRIS/ISA/ISA_TPD.tif') # Impervious data in /urbanVPRM_30m/driver_data/lc_isa/
ISA.dt = tifdt_fun(ISA_dat,"ISA")

## Merge LC and ISA
LC_ISA.dt = merge(LC.dt,ISA.dt,by=c("Index","x","y"))
npixel = as.numeric(nrow(LC_ISA.dt))

print(paste0("n. of pixels is ",npixel))
rm(ISA_dat,LC.dt,ISA.dt)

print("LC, ISA loaded!")


### SEE GREENUP_DORMANCY.R file for removing NA AND UNPHYSICAL VALUES ###

## Import Phenology data
# Growing Season calendar from resampled Multi Source Land Surface Phenology Product product (NASA; https://lpdaac.usgs.gov/products/mslsp30nav001/)
# 15% EVI increase
#greenup = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/greenup.tif') # Phenology data in /urbanVPRM_30m/driver_data/ms_lsp/
greenup = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD/ms_lsp/greenup_TPD.tif') # Phenology data in /urbanVPRM_30m/driver_data/ms_lsp/

# 85% EVI decrease
dormancy <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD/ms_lsp/dormancy_TPD.tif') # Phenology data in /urbanVPRM_30m/driver_data/ms_lsp/
SoGS.dt = tifdt_fun(greenup,"SOS")
EoGS.dt = tifdt_fun(dormancy,"EOS")
GS.dt = merge(SoGS.dt,EoGS.dt,by=c("Index","x","y"))

rm(greenup,dormancy,SoGS.dt,EoGS.dt)

## Landsat EVI and LSWI
LS_VI.dt = fread('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD/evi_lswi_interpolated_ls7and8.csv', data.table=FALSE) #EVI/LSWI data in /urbanVPRM_30m/driver_data/evi_lswi/

## Load EVI data for a reference (Fully forested) pixel
# Borden Pixel = 5043 This is just south of a road, maybe use pixel 6130 to see if it makes a difference (a little further from roads)
# TP39 Pixel = 5043
# TPD Pixel = 3440
EVI_ref = LS_VI.dt[which(LS_VI.dt$Index == 3440),]  
EVI_ref = EVI_ref$EVI_inter
minEVI_ref = min(EVI_ref)
EVI_ref = rep(EVI_ref,each=24)

#############################################################################

### Load script that defines model parameters and calculates fluxes
source("UrbanVPRM_code/VPRM_parameters_equations.R") # Parameters/equations script found in # Phenology data in /urbanVPRM_30m/scripts/
print("Get scale factors and GEE and Respiration fluxes")

## First define time period datatable. It will give the first 2 columns of the output data table.. 
dates = data.frame(as.POSIXct(seq(as.POSIXct(paste0(yr,"-01-01 00:00"),format="%Y-%m-%d %H:%M",tz="Etc/GMT+5"),
                                  as.POSIXlt(paste0(yr,"-12-31 23:00"),format="%Y-%m-%d %H:%M",tz="Etc/GMT+5"),by="hour")))
nhr = as.numeric(nrow(dates))
hoy = data.frame(1:nhr)
time.dt = cbind(dates,hoy)
colnames(time.dt) = c("Date","HoY")
rm(dates,hoy)

## Run Model

blocks = seq(1, npixel, by=nrow_block)

for(j in 1:length(blocks)) { 
  
  block = blocks[j]
  cat(paste0("\n Working on block: ", block))
  
  if(block != blocks[length(blocks)]){
    lim = (block+nrow_block)
  } else if (block == blocks[length(blocks)]) {
    lim = npixel+1
  }
  
  #if(length(blocks)>1){
  #  clima.dt = readRDS(paste0(dir_clima,"/rap_goes_",city,"_",yr,"_hourly_block_",sprintf("%08i",as.numeric(block)),".rds")) 
  #} else {
  #clima.dt = readRDS(paste0(dir_clima,"/rap_goes_",city,"_",yr,"_hourly.rds"))
  clima.dt = readRDS(paste0(dir_clima,"/rap_goes_TPD_",yr,"_hourly.rds"))
  #}
  

  output.dt = foreach(i=block:(lim-1)) %do% {
    #print(i)
    ### If EVI time series is made of NAs, skip to the next iteration/pixel
    if(all(is.na(LS_VI.dt$EVI[LS_VI.dt$Index==i]))){
      
      outputs = data.frame(Date=as.POSIXct(NA),HoY=NA_integer_,Index=i,GEE=NA_real_,
                           TScale=NA_real_,PScale=NA_real_,WScale=NA_real_,PAR=NA_real_,
                           Tair=NA_real_,Re=NA_real_,Ra=NA_real_,Rh=NA_real_,EVI_scale=NA_real_, stringsAsFactors=FALSE)
      
    } else {
      
      ### Otherwise, extract input data for pixel i
      ## Get Land Cover class for pixel i
      lc_which = which( LC_ISA.dt$Index==i )  
      lc_i = LC_lookup(as.numeric(LC_ISA.dt$LandCover[lc_which]),veg_type )
      
      ## Get mean ISA percentage for pixel i (must be between 0 and 100%)
      isa_i = as.numeric(LC_ISA.dt$ISA[lc_which]/100)
      if(is.na(isa_i) | isa_i == 127 | isa_i < 0 | isa_i == 2.55){
        isa_i = 0
      } else if(isa_i > 1){
        isa_i = 1
      }
      
      ## Flag water pixels
      if (lc_i == 'OTH'){wtr_i <- 1} else {wtr_i <- 0}
      
      ## Get hourly EVI and LSWI for pixel i
      ls_which = which(LS_VI.dt$Index==i)
      EVI_i = LS_VI.dt$EVI_inter[ls_which]     
      EVI_h = rep(EVI_i, each=24)
      LSWI_i = LS_VI.dt$LSWI_inter[ls_which]
      LSWI_h = rep(LSWI_i, each=24)
      rm(EVI_i,LSWI_i)
      
      ## Get hourly air temperature and shortwave radiation for pixel i
      clima_which = which(clima.dt$Index==i)
      tair_i = clima.dt$tmpC[clima_which] 
      swrad_i = clima.dt[clima_which,c("HoY","swRad")] 
      
      ### Finally get outputs for pixel i
      outputs = getFluxes(time.dt,i,lc_i,isa_i,wtr_i,EVI_h,LSWI_h,tair_i,swrad_i)
    }
    print(i)
    return(outputs)
  }
  
  output.dt = rbindlist(output.dt)
  
  output.dt$Date = as.Date(output.dt$Date,format="%Y%m%d")
  
  cat("\n Save data table with outputs..")
  
  if(length(blocks)>1){
    saveRDS(output.dt, paste0(dir_out,"/fluxes_",city,"_",yr,"_",veg_type,"_block_",
                              sprintf("%08i",as.numeric(block)),".rds"))
  } else {
    write.table(output.dt, "vprm_30m_SOLRIS_TPD_updated_R.csv",row.names = F,
                sep = ',')
  }
  
  cat(paste0("\n Wrote block ", block,"!"))
  
  rm(output.dt)
}


cat("\n Done!")

