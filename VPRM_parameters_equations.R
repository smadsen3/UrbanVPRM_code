## IAN SMITH
## iasmith [at] bu.edu

# This script defines VPRM parameters and equations used in Winbourne et al. 2021
# Directories in this script correspond to the structure of the computing cluster where model calculations were executed.


## Load libraries
library("data.table")
library("raster")

setwd("/projectnb/buultra/iasmith/VPRM_urban/")

## Model parameters from Mahadevan et al. 2008 & Optimization from Duke Forest

## Land cover types: evergreen, deciduous, mixed, shrublands (open and closed), savannas, (savanna, woody), croplands, grasslands, wetlands, "other"

VPRM_LCs = c("ENF", "DBF", "MXF", "SHB", "SVN", "SOY", "CRP", "GRS", "WET", "OTH", "URB")
## Optimization sites
# ENF: NOBS (boreal), NIWOT (montane coniferous), METOLIUS (ponderosa pine)
# ENF: (tropical): DONALDSON
# DBF: HARVARD or Duke
# MXF: HOWLAND
# SHB: Lucky Hills
# SVN: TONZI
# SOY: MEAD-S2
# CRP: MEAD (USED CORN)
# GRS: VAIRA
# WET: PEATLAND
# OTH: 0 fluxes (here used for water)
# URB: Urban

### Model's parameters 
#VPRM_DBF= c(0, 20, 40, 5, 570, 0.127, 0.271, 0.25) # Mahadevan et al. 2008 (Harvard Forest)
VPRM_DBF= c(0, 30, 40, 5, 863.43378, 0.09355, 0.1379, 1.09) # Winbourne et al. 2021 (Duke Forest)
VPRM_MXF= c(0, 20, 40, 2, 629, 0.123, 0.244, -0.24)
VPRM_URB= c(0, 20, 40, 2, 629, 0.123, 0.244, -0.24)
VPRM_SHB= c(2, 20, 40, 1, 321, 0.122, 0.028, 0.48)
# USING CORN FOR CRP
VPRM_SOY= c(5, 22, 40, 2, 2051, 0.064, 0.209, 0.20)
VPRM_CRP= c(5, 22, 40, 2, 1250, 0.075, 0.173, 0.82)
VPRM_GRS= c(2, 18, 40, 1, 542, 0.213, 0.028, 0.72)
VPRM_SVN= c(2, 20, 40, 1, 3241, 0.057, 0.012, 0.58)
VPRM_WET= c(0, 20, 40, 3, 558, 0.051, 0.081, 0.24)
# USING NIWOT FOR ENF
VPRM_ENF= c(0, 20, 40, 1, 446, 0.128, 0.250, 0.17)
VPRM_OTH= c(0, 0, 0, 0, 0, 0, 0, 0)

VPRM_param = rbind(VPRM_ENF, VPRM_DBF, VPRM_MXF, VPRM_SHB, VPRM_SVN, VPRM_SOY, VPRM_CRP, VPRM_GRS, VPRM_WET, VPRM_OTH, VPRM_URB)
VPRM_param = as.data.table(VPRM_param)

## Units are: T (°C); PAR(umole m-2 s-1); lambda (umole CO2 m-2 s-1/umole PAR m-2 s-1); alpha (umole CO2 m-2 s-1 / 0C); B (umole CO2 m-2 s-1); 
par_names=c("Tmin", "Topt", "Tmax", "Tlow", "PAR0", "lambda", "alpha", "beta")
setnames(VPRM_param, paste0("V", 1:8),par_names)
VPRM_param = cbind(VPRM_LCs, VPRM_param)
rm(VPRM_CRP,VPRM_DBF,VPRM_ENF,VPRM_GRS,VPRM_MXF,VPRM_OTH,VPRM_SHB,VPRM_SVN,VPRM_URB,VPRM_WET,VPRM_LCs,par_names)





## Aggregate and translate classes (Here everything is DBF, except water)
LC_lookup = function(LC,veg_class){
  if(LC %in% c(11)){
    return("OTH")
  } else {
    return(veg_class)
  }
}

## Functions to get scale factors
# Temperature scalar
getTScale = function(lc,tair){
  
  tmin = VPRM_param[VPRM_LCs==lc,Tmin]
  tmax = VPRM_param[VPRM_LCs==lc,Tmax]
  topt = VPRM_param[VPRM_LCs==lc,Topt]
  
  if(is.null(tair)){
    print("Invalid RAP air temperature data...")
    return(NULL)
  }

    TScale = rep(NA, length(tair))
for (i in 1:length(tair)){
if (is.na(tair[i])){TScale[i] <- NA} else
  if (tair[i] >= 20 & tair[i] <= 30){
    TScale[i] = 1
  } else if(tair[i] < 20){
  TScale[i] = ((tair[i] - tmin)*(tair[i]-tmax)) / 
    ((tair[i]-tmin)*(tair[i]-tmax) - (tair[i]-20)^2)
} else {
    TScale[i] = ((tair[i] - tmin)*(tair[i]-tmax)) / 
      ((tair[i]-tmin)*(tair[i]-tmax) - (tair[i]-20)^2)
}
}

  TScale[TScale<0] = 0
  TScale[TScale>1] = 1
  return(TScale) 
}
# Phenology scalar
getPScale = function(idx,EVI){
  SOS_i = GS.dt$SOS[GS.dt$Index==idx]
  EOS_i = GS.dt$EOS[GS.dt$Index==idx]
  EVI_min = min(EVI)    # Min and Max EVI within the whole year
  EVI_delta = max(EVI) - EVI_min
  PScale   = (EVI - EVI_min)/EVI_delta
  PScale[PScale<0] = 0
  PScale[PScale>1] = 1
  # Prior to bud burst (Start of Season), PScale set to 0
  PScale[1:(24*SOS_i)] <- 0
  # PScale Set to 0 during dormancy (after 85% decrease in greenness)
  PScale[(24*EOS_i):length(PScale)] <- 0
  return(PScale)
}
# Moisture Scalar
getWScale = function(idx,EVI,LSWI){
  
  SOS_i = GS.dt$SOS[GS.dt$Index==idx]
  EOS_i = GS.dt$EOS[GS.dt$Index==idx]
  SOS = (as.numeric(SOS_i)-1)*24
  EOS = (as.numeric(EOS_i)-1)*24
  LSWI_gsl = LSWI[SOS:EOS]
  
  LSWI_max = max(LSWI_gsl)
  rm(LSWI_gsl)
  
  WScale = (1+LSWI)/(1+LSWI_max)
  WScale[WScale < 0] = 0 
  WScale[WScale > 1] = 1 # WScale should be always between 0 and 1
  return(WScale)
}




## Function to calculate the GEE and Re fluxes. It gives a data table showing the hourly values of:
## GEE, Re, Ra, Rh, Pscale, Tscale, Wscale, PAR, tair, for pixel i

getFluxes = function(time,idx,lc,isa,wtr,EVI,LSWI,tair,swrad){
  
  ### Get GEE
  ## Model parameters for GEE calculation
  lmbd = VPRM_param[VPRM_LCs==lc,lambda]
  PAR0 = VPRM_param[VPRM_LCs==lc,PAR0]
  
  ## Define PAR (equation from Mahadevan et al 2008)
  swrad[which(swrad$swRad < 0,2),"swRad"]=0
  PAR = swrad$swRad/0.505
  
  ## Get scale factors
  TScale = getTScale(lc,tair)
  PScale = getPScale(idx,EVI)
  WScale = getWScale(idx,EVI,LSWI)

  ## Test if EVI causes neg values of GEE
  EVI[EVI<0]=0
  
  ## GEE equation
  GEE = lmbd * TScale * PScale * WScale * EVI * PAR / (1+PAR/PAR0)
  
  # Normalize GEE flux so sub-pixel water isn't counted..
  GEE = GEE * (1 - wtr)
  
  TScale = as.data.frame(as.numeric(TScale))
  PScale = as.data.frame(as.numeric(PScale))
  WScale = as.data.frame(as.numeric(WScale))
  PAR = as.data.frame(as.numeric(PAR)) 
  
  ## Define the column indicating the pixel ID
  indexcol = rep(idx,time=nrow(time))
  
  ## Merge outputs related to GEE flux
  result = cbind(time,indexcol,GEE,TScale,PScale,WScale,PAR,tair)
  
  ### Get Respiration
  ## Model parameters for Respiration calculation
  alpha = VPRM_param[VPRM_LCs==lc,alpha]
  beta = VPRM_param[VPRM_LCs==lc,beta]
  tlow = VPRM_param[VPRM_LCs==lc,Tlow]
  
  ## respiration occurs at rate defined by beta below freezing
  tair[tair<0] = 0
  
  ## Respiration is a linear function of air temperature
  Re = (alpha * tair) + beta
  
  ## Partition respiration into heterotrophic and autotrophic respiration
  Ra = 0.5*Re
  Rh = 0.5*Re
  
  if(isa > 0.05){
    ## Modify Re components: Rh is reduced by ISA and Ra is reduced by EVI 
    Rh = Rh * (1-isa)
    # Ra reduced by EVI, but rescaled to maintain min EVI
    EVI_scale = EVI/EVI_ref
    EVI_scale[!(EVI_ref>0.05)] = 1
    EVI_scale[EVI_scale>1] = 1
    EVI_scale[EVI_scale<0] = 0
    Ra = Ra * EVI_scale 
    EVI_scale = as.data.frame(as.numeric(EVI_scale))
  } else {
 	## if no ISA in pixel, no scaling
    EVI_scale = as.data.frame(rep(NA,time=length(Re)))
  }
  
  # Put the respiration components back together
  Re = Ra + Rh
  
  # Modify total respiration by percentage of water within the pixel
  Re = Re * (1-wtr)
  
  # In Winbourne et al. 2021, minimum ecosystem respiration is set to lowest observed value
  if(wtr == 0) {Re[which(Re < 1.7795)] <- 1.7795}
  
  ## Merge all outputs in the same data table 
  result = cbind(result,Re,Ra,Rh,EVI_scale) 
  colnames(result) = c("Date","HoY","Index","GEE","TScale","PScale","WScale","PAR","Tair","Re","Ra","Rh","EVI_scale")  
  
  return(result)
}
