# FOR SOME REASON THERE ARE NA VALUES IN PAR (SWRAD) this may be causing issues
# but there are NA values in PAR for the 30m resolution and there aren't issues there....
#Added pre-processing of GOES data to help fix this

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
library("foreach")
library("doParallel")
library("ggplot2") #not needed for running just visualizing

## register cores for parallel processes on computing cluster
cores = as.numeric(Sys.getenv("NSLOTS"))
if (is.na(cores)) cores=3
registerDoParallel(cores)
print(paste0("n. of cores is ",cores))

#setwd("/projectnb/buultra/iasmith/VPRM_urban_30m/")

setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files')

# Arguments: 
city = 'GTA_V061_500m_2021'
yr = 2021
veg_type = 'DBF' #Maybe use Mixed forest instead?

## If area is too big (n of pixels > nrow_block) divide in blocks of nrow_block cells
nrow_block=2500

# Climate data folder
dir_clima = paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/2021') # climate data in /urbanVPRM_30m/driver_data/rap_goes/

## Define the path to the folder where outputs are saved 
dir.create(paste0("outputs"), showWarnings = FALSE)

dir.create(paste0("outputs/",city), showWarnings = FALSE)
dir_out = paste0(city)

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
LC = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/LandCover/MODIS_V061_LC_GTA_500m_2021.tif') # Land cover data in /urbanVPRM_30m/driver_data/lc_isa/
LC.dt = tifdt_fun(LC,"LandCover")
#LC_NIST = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/NIST30/Landcover/LC_NIST.tif') # Land cover data in /urbanVPRM_30m/driver_data/lc_isa/
#LC_NIST.dt = tifdt_fun(LC_NIST,"LandCover")

ISA_dat = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/ISA/ISA_GTA_GMIS_Toronto_ACI_SOLRIS_500m_2021.tif') # Impervious data in /urbanVPRM_30m/driver_data/lc_isa/
ISA_dat<-resample(ISA_dat,LC) #for some reason x and y were offset by 1*10^-9 compared to LC raster, fix it by resampling
ISA.dt = tifdt_fun(ISA_dat,"ISA")

C4 = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/LandCover/C4_frac_GTA_500m_2021.tif') # C4 fraction
C4<-resample(C4,LC) #for some reason x and y were offset by 1*10^-9 compared to LC raster, fix it by resampling
C4.dt = tifdt_fun(C4,"C4")
## Merge LC and ISA
#LC_ISA.dt = merge(LC.dt,ISA.dt,by=c("Index","x","y")) 
#for some reason y is off by 1*10^-9 so it won't merge do it manually below:
LC_ISA.dt = merge(LC.dt,ISA.dt,by=c("Index","x"))
LC_ISA.dt = merge(LC_ISA.dt,C4.dt,by=c("Index","x"))
LC_ISA.test<-LC_ISA.dt[,1:2]
LC_ISA.test$y<-LC_ISA.dt$y.x
LC_ISA.test$LandCover<-LC_ISA.dt$LandCover
LC_ISA.test$ISA<-LC_ISA.dt$ISA
LC_ISA.test$C4<-LC_ISA.dt$C4

LC_ISA.dt<-LC_ISA.test
npixel = as.numeric(nrow(LC_ISA.dt))

print(paste0("n. of pixels is ",npixel))


wtr_dat = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/SOLRIS_aggregated_water_cover_GTA.tif') # Impervious data in /urbanVPRM_30m/driver_data/lc_isa/
wtr_dat<-resample(wtr_dat,LC) #for some reason x and y were offset by 1*10^-9 compared to LC raster, fix it by resampling
wtr.dt = tifdt_fun(wtr_dat,"wtr")

rm(ISA_dat,LC.dt,ISA.dt,C4,C4.dt,LC_ISA.test,wtr_dat)

print("LC, ISA & wtr loaded!")


### SEE GREENUP_DORMANCY.R file for removing NA AND UNPHYSICAL VALUES ###

## Import Phenology data
# Growing Season calendar from resampled Multi Source Land Surface Phenology Product product (NASA; https://lpdaac.usgs.gov/products/mslsp30nav001/)
# 15% EVI increase
#greenup = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/greenup.tif') # Phenology data in /urbanVPRM_30m/driver_data/ms_lsp/

greenup = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MODIS_V061_avg_greenup_2021.tif') # Phenology data in /urbanVPRM_30m/driver_data/ms_lsp/
greenup <- crop(greenup,LC) #crop greenup to be the same size as LC data

# 85% EVI decrease
dormancy <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MODIS_V061_avg_Dormancy_2021.tif') # Phenology data in /urbanVPRM_30m/driver_data/ms_lsp/
dormancy<- crop(dormancy,LC)
SoGS.dt = tifdt_fun(greenup,"SOS")
EoGS.dt = tifdt_fun(dormancy,"EOS")
GS.dt = merge(SoGS.dt,EoGS.dt,by=c("Index","x","y"))

rm(greenup,dormancy,SoGS.dt,EoGS.dt)

## Landsat EVI and LSWI
LS_VI.dt = fread('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/adjusted_evi_lswi_interpolated_modis_v061_qc_filtered_LSWI_filtered.csv', data.table=FALSE) #EVI/LSWI data in /urbanVPRM_30m/driver_data/evi_lswi/

#plot(LS_VI.dt$LSWI[LS_VI.dt$Index==11026])
#points(LS_VI.dt$LSWI_inter[LS_VI.dt$Index==11026],col='blue')
#points(LS_VI.dt$LSWI[LS_VI.dt$Index==11026],col='red',pch=16)

#Uncomment below to visualize
#x.stk<-NULL
#y.stk<-NULL
#Date.stk <- NULL
#EVI.stk <- NULL
#EVI_inter.stk<-NULL
#Index.stk<-NULL
#LSWI.stk<-NULL
#LSWI_inter.stk<-NULL
#LC.stk<-NULL

#for (I in 1:length(LS_VI.dt$Index)){
#  if (148<LS_VI.dt$DOY[I] & LS_VI.dt$DOY[I]<150){
#    if (length(x.stk)==0){
#      x.stk <- LS_VI.dt$x[I] 
#      y.stk <- LS_VI.dt$y[I]
#      Date.stk <- LS_VI.dt$DOY[I]
#      EVI.stk <- LS_VI.dt$EVI[I]
#      EVI_inter.stk <- LS_VI.dt$EVI_inter[I]
#      Index.stk<-LS_VI.dt$Index[I]
#      LSWI.stk <- LS_VI.dt$LSWI[I]
#      LSWI_inter.stk<-LS_VI.dt$LSWI_inter[I]
#      LC.stk<-LC_ISA.dt$LandCover[LS_VI.dt$Index[I]]
#    } else {
#      x.stk <- append(x.stk, LS_VI.dt$x[I])
#      y.stk <- append(y.stk, LS_VI.dt$y[I])
#      Date.stk <- append(Date.stk, LS_VI.dt$DOY[I])
#      EVI.stk <-append(EVI.stk, LS_VI.dt$EVI[I])
#      EVI_inter.stk <- append(EVI_inter.stk, LS_VI.dt$EVI_inter[I])
#      Index.stk <-append(Index.stk,LS_VI.dt$Index[I])
#      LSWI.stk <- append(LSWI.stk,LS_VI.dt$LSWI[I])
#      LSWI_inter.stk <- append(LSWI_inter.stk,LS_VI.dt$LSWI_inter[I])
#      LC.stk<-append(LC.stk,LC_ISA.dt$LandCover[LS_VI.dt$Index[I]])
#    }
#  }
#}

#All.data.evi <- data.frame(
#  Index = Index.stk[-length(x.stk)],
#  x = x.stk[-length(x.stk)], 
#  y = y.stk[-length(x.stk)],
#  EVI =EVI.stk[-length(x.stk)],
#  LSWI = LSWI.stk[-length(x.stk)],
#  EVI_inter =EVI_inter.stk[-length(x.stk)],
#  LSWI_inter = LSWI_inter.stk[-length(x.stk)],
#  LC = LC.stk[-length(x.stk)],
#  stringsAsFactors = FALSE
#)

#modcrs<-'+proj=longlat +datum=WGS84 +no_defs'
#s <- SpatialPoints(cbind(All.data.evi$x, All.data.evi$y), proj4string=CRS(modcrs))

#lonlat <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 

#coords <- spTransform(s, lonlat)
#evi_xy_data<-cbind(as.data.frame(coords), All.data.evi$EVI,All.data.evi$LSWI,All.data.evi$EVI_inter, All.data.evi$LSWI_inter, All.data.evi$LC)

#s2 <- SpatialPoints(cbind(All.data.evi$x[98],All.data.evi$y[98]), proj4string=CRS(modcrs))
#coords2 <- spTransform(s2, lonlat)

#Fpix=as.data.frame(coords2) #forested pixel (index=98)

##Borden
#s3 <- SpatialPoints(cbind(-79.9333,44.3167), proj4string=CRS(modcrs))
##TP39
##s3 <- SpatialPoints(cbind(-80.3574,42.7102), proj4string=CRS(modcrs))
##TPD
##s3 <- SpatialPoints(cbind(-80.5577,42.6353), proj4string=CRS(modcrs))
#coords3 <- spTransform(s3, lonlat)

#B=as.data.frame(coords3) #fluxtower location

#ggplot(evi_xy_data,                       # Draw ggplot2 plot
#       aes(x = coords.x1, y = coords.x2, width=1/240,
#           height=1/240)) + geom_tile(aes(fill=All.data.evi$LC)) + geom_point(data=Fpix,colour = 'red') + geom_point(data=B,colour = 'green') + coord_equal() + xlab(expression(Longitude ^o)) + ylab(expression(Latitude ^o)) + ggtitle('Borden Land Cover')


#ggplot(evi_xy_data,                       # Draw ggplot2 plot
#       aes(x = coords.x1, y = coords.x2, width=1/240,
#           height=1/240)) + geom_tile(aes(fill=All.data.evi$LC)) + geom_point(data=Fpix,colour = 'red') + coord_equal() + xlab(expression(Longitude ^o)) + ylab(expression(Latitude ^o)) + ggtitle('GTA Land Cover')

#ggplot(evi_xy_data,                       # Draw ggplot2 plot
#       aes(x = coords.x1, y = coords.x2, width=1/240,
#           height=1/240)) + geom_tile(aes(fill=All.data.evi$EVI_inter)) + geom_point(data=Fpix,colour = 'red') + coord_equal() + xlab(expression(Longitude ^o)) + ylab(expression(Latitude ^o)) + ggtitle('GTA Interpolated EVI')





## Load EVI data for a reference (Fully forested) pixel
# Borden Pixel = 98 # deciduous
# TP39 Pixel = 158 #deciduous or  109 #Mixed forest
# TPD Pixel = 153 #deciduous
# GTA Pixel = 3011 # deciduous
EVI_ref = LS_VI.dt[which(LS_VI.dt$Index == 3011),]  
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
  clima.dt = readRDS(paste0(dir_clima,"/rap_goes_",city,"_hourly_fixed.rds"))#v3_cropped.rds"))
  #clima.dt = readRDS(paste0(dir_clima,"/rap_goes_GTA_500m_",yr,"_hourly.rds"))
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
      lc_i = LC_lookup(as.numeric(LC_ISA.dt$LandCover[lc_which]),as.numeric(LC_ISA.dt$C4[lc_which]),veg_type )
      
      ## Get mean ISA percentage for pixel i (must be between 0 and 100%)
      isa_i = as.numeric(LC_ISA.dt$ISA[lc_which]/100)
      if(is.na(isa_i) | isa_i == 127 | isa_i < 0 | isa_i == 2.55){
        isa_i = 0
      } else if(isa_i > 1){
        isa_i = 1
      }
      
      ## Flag water pixels
      if (lc_i == 'OTH'){wtr_i <- 1} else {wtr_i <- as.numeric(wtr.dt$wtr[lc_which])}
      
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
    write.table(output.dt, paste0("GTA_V061_500m_2021/vprm_GMIS_Toronto_ACI_SOLRIS_ISA_500m_GTA_V061_2021_no_PScale_adjusted_Topt_Ra_URB_parameters_fixed_gapfilled_LSWI_filtered_block_",sprintf("%08i",as.numeric(block)),".csv"),
                row.names = F, sep = ',')
    #saveRDS(output.dt, paste0(dir_out,"/fluxes_",city,"_",yr,"_",veg_type,"_block_",
    #                          sprintf("%08i",as.numeric(block)),".rds"))
  } else {
    write.table(output.dt, "GTA_V061_500m_2021/vprm_GMIS_Toronto_ACI_SOLRIS_ISA_500m_GTA_V061_2021_no_PScale_adjusted_Topt_Ra_URB_parameters_fixed_gapfilled_LSWI_filtered.csv",row.names = F,
                sep = ',')
  }
  
  cat(paste0("\n Wrote block ", block,"!"))
  
  rm(output.dt)
}


cat("\n Done!")

