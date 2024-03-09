library(raster)
library(dplyr)
library(ncdf4)
library(data.table)
library(ggplot2)
library(parallel)
library(StreamMetabolism) #for sunrise/sunset times
library(lubridate) #for converting date formats
library(zoo)

##GOES.dt_ex = readRDS("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_500m_2019/2019/rap_goes_GTA_500m_2019_hourly_excellent_only.rds")
##GOES.dt = readRDS("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_500m_2019/2019/rap_goes_GTA_500m_2019_hourly_test.rds")

##x_test <- GOES.dt$x[GOES.dt$HoY==2950]
##y_test <- GOES.dt$y[GOES.dt$HoY==2950]

###Select an hour of the year to plot GOES data
##sw <- GOES.dt$swRad[GOES.dt$HoY==3021]#4118]#7074]#2470]
##sw_ex <- GOES.dt_ex$swRad[GOES.dt_ex$HoY==3021]#4118]#7074]#2470]
##Ind <- GOES.dt$Index[GOES.dt$HoY==3021]#4119]#7074]#2470]
##GOES_x <- GOES.dt$x[GOES.dt$HoY==3021]#4119]#7074]#2470
##GOES_y <- GOES.dt$y[GOES.dt$HoY==3021]#4119]

###Convert it into a dataframe for plotting
##GOES.df <- data.frame(x=x_test,y=y_test,swRad=sw,swRad_ex=sw_ex)

##ggplot(GOES.df, aes(x=x,y=y,col=swRad),main='HOY=3021')+geom_point()
##ggplot(GOES.df,                       # Draw ggplot2 plot
##       aes(x = x_test, y = y_test, width=1/240,
##           height=1/240)) + geom_tile(aes(fill=swRad))+ coord_equal() + ggtitle('GOES SSRD (T = 3021 hrs)')

##sw_test <- GOES.dt$swRad[GOES.dt$Index==225]#1596]
##sw_test_ex <- GOES.dt_ex$swRad[GOES.dt_ex$Index==225]#1596]
##tmp_test <- GOES.dt$tmpC[GOES.dt$Index==225]#1596]
##tmp_test_fill <- GOES.dt$tmpC_inter[GOES.dt$Index==225]#1596]
##HoY_test <- GOES.dt$HoY[GOES.dt$Index==225]#1596]

##df <- data.frame(HoY=HoY_test,swRad=sw_test, swRad_ex=sw_test_ex)#,tmpC=tmp_test)#, tmp_inter=tmp_test_fill)
##plot(df$HoY[3000:3050],df$swRad[3000:3050],col='black', type='o',pch=16)
##points(df$HoY[4115:4125],df$swRad_ex[4115:4125],col='black', type='o',pch=16)

##GOES_raster<-rasterFromXYZ(GOES.df) 
##plot(GOES_raster$swRad)


#input.path  <- file.path('C:/Users/kitty/Documents/Research/SIF/SMUrF/data')
#SSRD.field <-'ERA5'
#yr<-'2019'
#SSRD.path  <- file.path(input.path, SSRD.field, yr,"Editted_SSRD") 
#SSRD.varname <-'SSRD'
#
#ERA5.files <- list.files(SSRD.path,SSRD.varname,full.names = T, recursive = T)

##doy.df <- get.doy.from.timestr(timestr, nday = 1, nhrs = nhrs, tz = tz)
##seq.dates <- seq(as.POSIXct(doy.df$min.timestr, tz = tz, format = '%Y%m%d%H'), 
##                 as.POSIXct(doy.df$max.timestr, tz = tz, format = '%Y%m%d%H'), 
##                 by = 1 * 60 * 60)    # by in second

#ERA5.file <-NULL
#for (m in 1:12){
#  if (m<10){
#    tmp.file <-ERA5.files[grepl(paste0('20190',m),ERA5.files)]
#  }else{
#    tmp.file <-ERA5.files[grepl(paste0('2019',m),ERA5.files)]
#  }
#  ERA5.file <- c(ERA5.file, tmp.file)
#  #print(tmp.file)
#  #print(m)
#}

## UNCOMMENT TO DO ALL DATA
##ERA5.stk <- NULL
##for (f in 1:length(ERA5.file)){
##  tmp.times <- ncvar_get(nc_open(ERA5.file[f]),'time')
##  tmp.end.dates <- as.POSIXct(tmp.times * 3600, origin = '1900-01-01 00:00:00', 
##                              tz = 'UTC')
##  # !!!! hours in ERA5 stands for the ending hours
##  # so we now move it to the starting hour, DW, 05/26/2020 
##  tmp.start.dates <- tmp.end.dates - 1 * 3600
##  ## locate the correct time period, read SSRD as rasterStack
##  #tmp.bands <- which(tmp.start.dates %in% seq.dates)
##  tmp.stk <- raster::stack(ERA5.file[f])#, bands = tmp.bands) 
##  
##  # the dates that raster processed are WRONG!!!
##  names(tmp.stk) <- tmp.start.dates#[tmp.bands] 
##  
##  # store data 
##  if (f == 1) ERA5.stk <- tmp.stk
##  if (f >  1) ERA5.stk <- stack(ERA5.stk, tmp.stk)
##}

#tmp.times <- ncvar_get(nc_open(ERA5.file[5]),'time')
#tmp.end.dates <- as.POSIXct(tmp.times * 3600, origin = '1900-01-01 00:00:00', 
#                            tz = 'UTC')
## !!!! hours in ERA5 stands for the ending hours
## so we now move it to the starting hour, DW, 05/26/2020 
#tmp.start.dates <- tmp.end.dates - 1 * 3600
### locate the correct time period, read SSRD as rasterStack
##tmp.bands <- which(tmp.start.dates %in% seq.dates)
#tmp.stk <- raster::stack(ERA5.file[5])#, bands = tmp.bands) 

## the dates that raster processed are in the WRONG format!!!
#names(tmp.stk) <- tmp.start.dates#[tmp.bands]

##ext <- raster::extent(-79.7, -79.1, 43.5, 43.9)

# define study domain, city and year
#xmin = -79.9333-4/240
#xmax = -79.9333+4/240
#ymin = 44.3167-4/240
#ymax = 44.3167+4/240
#city = 'Borden_V061_500m_2020'

#xmin = -80.3574-4/240
#xmax = -80.3574+4/240
#ymin =  42.7102-4/240
#ymax =  42.7102+4/240
#city = "TP39_V061_500m_2019"

#xmin = -80.5577-4/240
#xmax = -80.5577+4/240
#ymin =  42.6353-4/240
#ymax =  42.6353+4/240
#city = "TPD_V061_500m_2019"

#Toronto:
xmin = -79.7
xmax = -79.1
ymin =  43.5
ymax =  43.9

# Create slightly larger bounding box to crop to (avoids dropped pixels at edge when reprojecting / resampling)
bbox <- extent(xmin,xmax,ymin,ymax)
buff=0.05
bbox.extra <- extent(xmin - buff, xmax + buff, ymin - buff, ymax + buff)

#tmp.stk <-crop(tmp.stk,bbox.extra)

#tmp.stk <- tmp.stk/3600
#tmp.stk[tmp.stk< -1000]<-NA #remove fil?? (CHECK THIS IS A FILL VALUE)

##data_test<-rasterToPoints(tmp.stk$X2019.06.20.15.00.00)
##data_test<-as.data.frame(data_test)
##names(data_test) <- c('x','y','SSRD')

##coords <- SpatialPoints(cbind($x, All.data.MOD$y), proj4string=CRS(sincrs))

##ERA5_xy_data<-cbind(as.data.frame(coords), All.data.MOD$GEE, All.data.MOD$Re,
##                       #All.data.MOD$Ra, All.data.MOD$Rh, All.data.MOD$T_Scale,
##                       All.data.MOD$P_Scale) #, All.data.MOD$W_Scale, 

##ggplot(data_test,                       # Draw ggplot2 plot
##       aes(x = x, y = y)) + geom_tile(aes(fill=SSRD))+ coord_equal() + ggtitle('ERA5 SSRD (2019/06/20 15:00)')


#plot(tmp.stk$X2019.05.20.15.00.00,main='ERA5 SSRD, 2019/05/20, 15:00')

yr = 2021

GOES_CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "

# Create extended bounding box raster in GOES projection 
gridXY = as(raster::extent(bbox.extra), "SpatialPolygons")
proj4string(gridXY) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
GOES.XY <- projectRaster(raster(gridXY),crs=GOES_CRS)

inDIR <-'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GOES/2021/'
rl <- list.files(path=inDIR,pattern='GOES') # GOES data downloaded from ftp://eftp.ifremer.fr/cersat-rt/project/osi-saf/data/radflux/

##Start list with the last day of April to match ERA5 data
##idx_1<-grepl('2019043023',rl)

#GOES.list<-NULL
##GOES.stk <- raster(paste0(inDIR,rl[idx_1]), varname = 'ssi')
##GOES.crop<-crop(GOES.stk,extent(GOES.XY))
##GOES.proj <- resample(GOES.crop,tmp.stk)
##GOES.list<- append(GOES.list,GOES.proj[2,3])

##NOTE: THERE ARE 3 MISSING HOURS IN MAY
##idx <- grepl('201905',rl)
##GOES.files<-rl[idx]
##GOES.files<-GOES.files[1:740] #remove the last dataset

##GOES.test<-NULL
#for (f in GOES.files){
#  GOES.stk <- raster(paste0(inDIR,f), varname = 'ssi')
#  #GOES_QF <- raster(paste0(inDIR,f), varname = 'ssi_confidence_level')
#  #GOES.stk[GOES_QF<3]<-NA
#  GOES.crop<-crop(GOES.stk,extent(GOES.XY))
#  GOES.proj <- resample(GOES.crop,tmp.stk)
#  GOES.list<- append(GOES.list,GOES.proj[2,3])
#  #print(f)
#}

##Compare data in time
##Missing GOES data on 20190513040000-20190513060000 (inclusive) (index ~294)
#plot(values(tmp.stk)[9,][0:500],type='o',pch=16,col='black',main='Black: ERA5, Red: GOES, SSRD, May 2019')
#points(GOES.list[0:500],type='o',col='red')

times <- fread(paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/RAP/2021/times',yr,'.csv')) # times data in /urbanVPRM_30m/driver_data/times/

#GOES_raster<- function(dir,file){
#  if(file.exists(paste0(dir,file))){
#    print(paste0("Processing file", file))
#    rs <- raster(paste0(dir,file),varname = 'ssi')
#    rs_c <- raster(paste0(dir,file),varname = 'ssi_confidence_level')
#    goes.crop <- crop(rs,bbox.extra)
#    goes_QF <-copy(goes.crop)
#    goes_QF[goes_QF<3]<-NA #remove any data with confidence level less than acceptable
#    #goes_proj <- resample(goes_3,GOES.XY)
#    #goes_stk<-stack(goes.crop,goes_3)
#    #names(goes_stk)<-c(ssi,ssi_QC)
#    return(goes_QF)
#  }
#}
  
GOES_raster<- function(dir,dt){
  name_list<-NULL
  file<-rl[grep(as.character(dt),rl)]
  if(file.exists(paste0(dir,file))){
    print(paste0("Processing file ", file))
    rs <- raster(paste0(dir,file),varname = 'ssi')
    rs_c <- raster(paste0(dir,file),varname = 'ssi_confidence_level')
    goes.crop <- crop(rs,bbox.extra)
    goes_QF <-copy(goes.crop)
    goes_QF[goes_QF<3]<-NA #remove any data with confidence level less than acceptable
    #goes_proj <- resample(goes_3,GOES.XY)
    #goes_stk<-stack(goes.crop,goes_3)
    #names(goes_stk)<-c(ssi,ssi_QC)
    names(goes_QF)<-substr(file,1,10)
  }else{
    print(paste0("Accounting for missing file ", dt))
    rs <- raster(paste0(dir,rl[1]),varname = 'ssi')
    goes.crop <- crop(rs,bbox.extra)
    goes_QF <-copy(goes.crop)
    goes_QF<-goes_QF*NA
    names(goes_QF)<-as.character(dt)
  }
  return(goes_QF)
}


outlist <- mcmapply(GOES_raster, dir=inDIR, dt=times$datetime, mc.cores=1)

names(outlist)<-as.character(times$datetime)
#names(GOES.proj)<-substr(rl[idx_1],1,10)
st<-stack(outlist)
#cnames <-names(st)
#cnames[1] <-"X2019043023"
#names(st)<-cnames
cnames<-names(outlist)

dt<-as.data.table(as.data.frame(st,xy=T))
setnames(dt,c('x','y',cnames))
dt <-setDT(dt)
dm <- melt.data.table(dt,id.vars=c('x','y'),variable.name='datetime',value.name='swrad',variable.factor=F)
dm[,datetime:=as.character(datetime)][,swrad:=as.numeric(swrad)]
setkey(dm,x,y,datetime)

#city='GTA_500m_2019'
#outDIR <- paste0(city,'/',yr)
#rap2 <- readRDS(paste0(outDIR,'/rap_',city,'.rds'))#'_',yr,'.rds'))
#setkey(rap2,x,y,datetime)
#rap2 <- dm[rap2]
#td <- times[,.(datetime,hour)]
#td$datetime <- as.character(td$datetime)
#setkey(td,datetime)
#setkey(rap2,datetime)
#rap2 <- td[rap2]

centX <- mean(xmin-buff,xmax+buff)
centY <- mean(ymin-buff,ymax+buff)
# 
sun.rise <- function(x){
  y <- sunrise.set(centY,centX,paste(substr(x,1,4),substr(x,5,6),substr(x,7,8),sep='/'),timezone='UTC')[1]
  return(y)
}
sun.set <- function(x){
  y <- sunrise.set(centY,centX,paste(substr(x,1,4),substr(x,5,6),substr(x,7,8),sep='/'),timezone='UTC')[2]
  return(y)
}


#times<-times[2880:3624] #select only May

#Deal with missing night time data
sunrise <- melt.data.table(as.data.table(lapply(times$datetime,sun.rise)),variable.name = 'date', value.name='posTime')
sunrise[,chr:=seq(length(unique(times$chr)))][,riseTime:=as.numeric(substr(as.character(ymd_hms(posTime)),12,13))]
sunrise$date<-substr(sunrise$posTime,1,10)
sunrise<-sunrise[,.(chr,riseTime)]
#sunrise<-sunrise[,.(chr,riseTime)]
setkey(sunrise,chr)

sunset <- melt.data.table(as.data.table(lapply(times$datetime,sun.set)),variable.name = 'date',value.name='posTime')
sunset[,chr:=seq(length(unique(times$chr)))][,setTime:=as.numeric(substr(as.character(ymd_hms(posTime)),12,13))]
sunset$date<-substr(sunset$posTime,1,10)
sunset<-sunset[,.(chr,setTime)]
#sunset <- sunset[,.(chr,setTime)]
setkey(sunset,chr)

#dm2<-copy(dm)
dm[,chr := .GRP, by = .(datetime)]
setkey(dm,chr)
dm <- sunrise[dm]
dm <- sunset[dm]
invisible(gc())

dm[swrad<=0,swrad:=NA]

dm[is.na(swrad) & setTime<riseTime & as.numeric(substr(datetime,9,10))<=riseTime+1 & as.numeric(substr(datetime,9,10))>=setTime-1,swrad:=0]
#dm[is.na(swrad) & setTime>riseTime & as.numeric(substr(datetime,10,11))<=riseTime+1]

dm[is.na(swrad) & setTime>riseTime & as.numeric(substr(datetime,9,10))<=riseTime+1,swrad:=0]
dm[is.na(swrad) & setTime>riseTime & as.numeric(substr(datetime,9,10))>=setTime-1,swrad:=0]



setorder(dm,y,x,chr)
len <- dim(dm[chr==1])[1]
dm[,Index := .GRP, by = .(x,y)]
dm <- dm[,.(Index,x,y,datetime,chr,swrad)]
#rap2[,tempK:=round(tempK,2)][,swrad:=round(swrad,2)]
setnames(dm,'chr','HOY')

dm$sw_inter<-dm$swrad

for (i in unique(dm$Index)){#[1:length(unique(clima.dt$Index))]){
  ind<-which(dm$Index==i)
  #clima.dt$sw_inter[ind]<-na.spline(clima.dt$sw_inter[ind])
  dm$sw_inter[ind]<-na.spline(dm$sw_inter[ind])
  ##if (i==1){
  ##  ind_length<-sum(is.na(clima.dt$swRad[ind]))
  ##}else{
  ##  ind_length<-append(ind_length,sum(is.na(clima.dt$swRad[ind])))
  ##}
  print(i)
} 


dm$sw_test<-dm$sw_inter

#Remove interpolated values that are above or below measured values
#hrs<-unique(dm$HOY[dm$sw_inter < -0.01])
hrs<-unique(dm$HOY[dm$sw_inter < min(dm$swrad,na.rm=TRUE) | dm$sw_inter > max(dm$swrad,na.rm=TRUE)])
for (h in hrs) {
  #idx<-which(dm$sw_inter<0 & dm$HOY==h)
  idx<-which((dm$sw_inter< min(dm$swrad,na.rm=TRUE) | dm$sw_inter> max(dm$swrad,na.rm=TRUE)) & dm$HOY==h)
  for (i in idx){
    id<-which(dm$x <= dm$x[i]+0.06 & dm$x >= dm$x[i]-0.06 & dm$y <= dm$y[i]+0.06 & dm$y >= dm$y[i]-0.06 & dm$HOY==h)
    #xvals<-unique(dm$x[dm$x <= dm$x[i]+0.06 & dm$x >= dm$x[i]-0.06])
    #yvals<-unique(dm$x[dm$x <= dm$x[i]+0.06 & dm$x >= dm$x[i]-0.06])
    #print(c(dm$x[i],id))#vals))
    n<-sum(is.na(dm$swrad[id]))
    if(n>=length(id)-1){#if there is 1 or less pixel that is not NA
      dm$sw_test[i]<-mean(dm$swrad[dm$HOY==h],na.rm=TRUE) #take the mean of the entire scene
      #print(paste(h,i,'scene',sep=' '))
    }else{
      dm$sw_test[i]<-mean(dm$swrad[id],na.rm=TRUE)
    }
    #dm$sw_test[i]<-mean(dm$swrad[id],na.rm=TRUE)
    #print(c(h,i,id))
  }
  print(h)
  #for (i in length(dm$x[idx])){
  #  print(paste0("x: "+str(dm$x[idx][i])+"surrounding: "+str(vals[i])))
  #}
  
}

#replace any remaining values outside of the range with the minimum and maximum of the range
dm$sw_test[dm$sw_inter < min(dm$swrad,na.rm=TRUE) & is.na(dm$sw_test)] <-0 
dm$sw_test[dm$sw_inter > max(dm$swrad,na.rm=TRUE) & is.na(dm$sw_test)] <- max(dm$swrad,na.rm=TRUE)

#dm$sw_test[dm$sw_test<0]<-0
#dm$sw_inter[dm$sw_inter<0]<-0

dir.create('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/pre_processed_GOES/',showWarnings=FALSE)
#city<-'GTA_V061_500m_2018'
saveRDS(dm,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/pre_processed_GOES/goes_GTA_2021_pre_processed_mean_filling_test_NA_rm.rds')


##Compare data in space
#GOES.stk <- raster(paste0(paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GOES/2019/origTIFF/'),'20190620150000-OSISAF-RADFLX-01H-GOES16.nc'), varname = 'ssi')
#GOES.crop<-crop(GOES.stk,extent(GOES.XY))
#GOES.proj <- resample(GOES.crop,tmp.stk$X2019.06.20.15.00.00)
#
#GOES_ERA_diff<-GOES.proj-tmp.stk$X2019.06.20.15.00.00

#GOES_ERA_diff.df<-rasterToPoints(GOES_ERA_diff)
#GOES_ERA_diff.df<-as.data.frame(GOES_ERA_diff.df)
#names(GOES_ERA_diff.df) <- c('x','y','SSRD')

#ggplot(GOES_ERA_diff.df,                       # Draw ggplot2 plot
#       aes(x = x, y = y)) + geom_tile(aes(fill=SSRD))+ coord_equal() + ggtitle('GOES-ERA5 SSRD (2019/06/20 15:00)')
