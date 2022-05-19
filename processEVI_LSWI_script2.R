## IAN SMITH
## iasmith [at] bu.edu

# This script calculates, and interpolates daily EVI and LSWI values for each 
# pixel in the study domain from Landsat 7/8 surface reflectance data
# This script creates the file evi_lswi_interpolated_ls7and8.csv used in 
# Winbourne et al. 2021
# Directories in this script correspond to the structure of the computing 
# cluster where model calculations were executed.
# To run this code, file paths and directories will need to be restructured to 
# import/write files
# Landsat 7 and 8 Collection 1 surface reflectance data were downloaded from 
# Google Earth Engine and cropped rasters available on REPOSITRORY in /driver_data/landsat 

# import packages
library("data.table")
library("raster")
library("sp")
library("rgdal")
library("lubridate")

#ACI:
LC_dat<-raster("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden/LandCover/ACI_unedited_LC_Borden.tif")
#SOLRIS:
#LC_dat<-raster("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden_SOLRIS/LandCover/LC_Borden.tif")
## Calculate EVI and LSWI indices for Landsat images that have been cropped to the study domain

# LANDSAT 8 files
setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden/landsat/landsat8') # landsat data in /urbanVPRM_30m/driver_data/landsat/
ls8 <- list.files()
for (i in 1:length(ls8)){
  ## read in raster stack
  file <- stack(ls8[i])
  ## apply scale factors for reflectance data in the bands needed for EVI/LSWI calculation
  file[[2]] <- file[[2]] * 0.0001
  file[[4]] <- file[[4]] * 0.0001
  file[[5]] <- file[[5]] * 0.0001
  file[[6]] <- file[[6]] * 0.0001
  ## only keep clear observations determined from CFmask (pixel QA band 11 codes 322, 386, 834, 898, or 1346)
  clear_code <- c(322, 386, 834, 898, 1346)
  bad_pixels <- which(!(values(file[[11]]) %in% clear_code))
  values(file[[2]])[bad_pixels] <- NA
  values(file[[4]])[bad_pixels] <- NA
  values(file[[5]])[bad_pixels] <- NA
  values(file[[6]])[bad_pixels] <- NA
  ## calculate EVI
  file[[13]] <- 2.5 * ((file[[5]] - file[[4]]) / (file[[5]] + 6 * file[[4]] - 7.5 * file[[2]] + 1))
  ## calculate LSWI
  file[[14]] <- (file[[5]] - file[[6]]) / (file[[5]] + file[[6]])
  ## convert raster data to data.table and assign Day of Year values
  ## EVI
  EVI.dt = as.data.table(as.data.frame(file[[13]], xy=T))
  EVI.dt = cbind(1:ncell(file[[13]]), EVI.dt)
  setnames(EVI.dt,c("Index","x","y", "EVI"))
  setkey(EVI.dt,Index,x,y)
  ## LSWI
  LSWI.dt = as.data.table(as.data.frame(file[[14]], xy=T))
  LSWI.dt = cbind(1:ncell(file[[14]]), LSWI.dt)
  #LSWI.dt$DOY = yday(ymd(paste0('2018',substr((names(file))[1],3,6))))
  #The line above was trying to use the first column of the file instead of the
  # file name (replacement bellow uses file name)
  #TPD:
  #LSWI.dt$DOY = yday(ymd(paste0('2018',substr(ls8[i],12,15))))
  #Borden:
  LSWI.dt$DOY = yday(ymd(paste0('2018',substr(ls8[i],3,6))))
  setnames(LSWI.dt,c("Index","x","y", "LSWI","DOY"))
  setkey(LSWI.dt,Index,x,y)
  ## save a data.table for each image
  assign(paste0('EVI_LSWI',i,'_8.dt'), merge(EVI.dt,LSWI.dt,by=c("Index","x","y")))
}

# landsat 7
setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden/landsat/landsat7')
ls7 <- list.files()
for (i in 1:length(ls7)){
  ## read in raster data
  file <- stack(ls7[i])
  ## apply scale factors for reflectance data in the bands needed for EVI/LSWI calculation
  file[[1]] <- file[[1]] * 0.0001
  file[[3]] <- file[[3]] * 0.0001
  file[[4]] <- file[[4]] * 0.0001
  file[[5]] <- file[[5]] * 0.0001
  ## Only keep clear observations (QA band 10 codes 66 or 130)
  clear_code <- c(66, 130)
  bad_pixels <- which(!(values(file[[10]]) %in% clear_code))
  values(file[[1]])[bad_pixels] <- NA
  values(file[[3]])[bad_pixels] <- NA
  values(file[[4]])[bad_pixels] <- NA
  values(file[[5]])[bad_pixels] <- NA
  ## calculate EVI
  file[[12]] <- 2.5 * ((file[[4]] - file[[3]]) / (file[[4]] + 6 * file[[3]] - 7.5 * file[[1]] + 1))
  ## calculate LSWI
  file[[13]] <- (file[[4]] - file[[5]]) / (file[[4]] + file[[5]])
  ## convert raster data to data.table and assign Day of Year values
  ## EVI
  EVI.dt = as.data.table(as.data.frame(file[[12]], xy=T))
  EVI.dt = cbind(1:ncell(file[[12]]), EVI.dt)
  setnames(EVI.dt,c("Index","x","y", "EVI"))
  setkey(EVI.dt,Index,x,y)
  ## LSWI
  LSWI.dt = as.data.table(as.data.frame(file[[13]], xy=T))
  LSWI.dt = cbind(1:ncell(file[[13]]), LSWI.dt)
  #LSWI.dt$DOY = yday(ymd(paste0('2018',substr((names(file))[1],3,6))))
  # Line above did not work, see comment in previous loop
  #TPD:
  #LSWI.dt$DOY = yday(ymd(paste0('2018',substr(ls7[i],12,15))))
  #Borden:
  LSWI.dt$DOY = yday(ymd(paste0('2018',substr(ls7[i],3,6))))
  setnames(LSWI.dt,c("Index","x","y", "LSWI","DOY"))
  setkey(LSWI.dt,Index,x,y)
  ## save a data.table for each image
  assign(paste0('EVI_LSWI',i,'_7.dt'), merge(EVI.dt,LSWI.dt,by=c("Index","x","y")))
}

## merge daily files into one for all of 2018 
## MAKE SURE TO CHANGE FOR EACH SITE (some locations have more/fewer overpasses)
EVI_LSWI.dt <- rbind(EVI_LSWI1_7.dt,EVI_LSWI2_7.dt,EVI_LSWI3_7.dt,EVI_LSWI4_7.dt,EVI_LSWI5_7.dt,EVI_LSWI6_7.dt,
                     EVI_LSWI7_7.dt,EVI_LSWI8_7.dt,EVI_LSWI9_7.dt,EVI_LSWI10_7.dt,EVI_LSWI11_7.dt, EVI_LSWI12_7.dt,
                     EVI_LSWI13_7.dt,EVI_LSWI14_7.dt,
                     EVI_LSWI1_8.dt,EVI_LSWI2_8.dt,EVI_LSWI3_8.dt,EVI_LSWI4_8.dt,EVI_LSWI5_8.dt,EVI_LSWI6_8.dt,
                     EVI_LSWI7_8.dt,EVI_LSWI8_8.dt,EVI_LSWI9_8.dt,EVI_LSWI10_8.dt,EVI_LSWI11_8.dt,EVI_LSWI12_8.dt,
                     EVI_LSWI13_8.dt,EVI_LSWI14_8.dt)

## order by DOY
EVI_LSWI.dt <- EVI_LSWI.dt[order(EVI_LSWI.dt$DOY),]

## convert to dataframe
EVI_LSWI.df <- as.data.frame(EVI_LSWI.dt)

## There are a few erroneous values with EVI = 2.5. These are omitted from the dataset 
EVI_LSWI.df[which(EVI_LSWI.df$EVI > 1),'EVI'] <- NA

## Set up new dataframe for EVI/LSWI Interpolation
npixel <- length(unique(EVI_LSWI.df$Index))
inter_evi_lswi <- as.data.frame(matrix(ncol = 2, nrow = npixel*365))
colnames(inter_evi_lswi) <- c('Index', 'DOY')
inter_evi_lswi[,1] <- rep(seq(1,npixel,1), 365)
inter_evi_lswi <- inter_evi_lswi[order(inter_evi_lswi[,1]),]
inter_evi_lswi[,2] <- rep(seq(1,365,1), npixel)
inter_evi_lswi <- merge(inter_evi_lswi, EVI_LSWI.df[,c('Index', 'EVI', 'LSWI', 'DOY')], by = c('Index', 'DOY'), all = TRUE)
inter_evi_lswi  <- inter_evi_lswi[order(inter_evi_lswi[,'Index'],inter_evi_lswi [,'DOY']),]

# add coordinates
x <- as.data.frame(tapply(EVI_LSWI.df$x, EVI_LSWI.df$Index, mean, na.rm = T))
y <- as.data.frame(tapply(EVI_LSWI.df$y, EVI_LSWI.df$Index, mean, na.rm = T))
xy <- cbind(as.numeric(rownames(x)), x, y)
colnames(xy) <- c('Index', 'x', 'y')

inter_evi_lswi <- merge(inter_evi_lswi, xy, by = 'Index', all = TRUE)
inter_evi_lswi <- inter_evi_lswi[,c('Index', 'DOY', 'x', 'y', 'EVI', 'LSWI')]

# Interpolate daily EVI values for each pixel
inter_evi_lswi$EVI_inter <- NA

for(i in unique(inter_evi_lswi$Index)){
  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$EVI)),'EVI']) > 3){
    pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
    spl <- with(pix[!is.na(pix$EVI),],smooth.spline(DOY,EVI, spar = .25)) 
    inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- predict(spl, c(1:365))$y
  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- NA}
  print(round(i/npixel*100, 1))
}

#inter_evi_lswi$EVI_inter2 <- NA


#for(i in 1:length(inter_evi_lswi$Index)){
#  #if(inter_evi_lswi$DOY[i]<50 
#  #  & length(inter_evi_lswi[which(inter_evi_lswi$Index == inter_evi_lswi$Index[i]
#  #                                 & inter_evi_lswi$DOY<inter_evi_lswi$DOY[i]+50 
#  #                                 & inter_evi_lswi$DOY>0 
#  #                                 & is.finite(inter_evi_lswi$EVI)),'EVI']) >= 1){
#  #    inter_evi_lswi$EVI_inter2[i] <-inter_evi_lswi$EVI_inter[i]
#  #  }
#  #else if(inter_evi_lswi$DOY[i]>315 
#  #        & length(inter_evi_lswi[which(inter_evi_lswi$Index == inter_evi_lswi$Index[i]
#  #                                      & inter_evi_lswi$DOY>inter_evi_lswi$DOY[i]-50 
#  #                                      & inter_evi_lswi$DOY<366 
#  #                                      & is.finite(inter_evi_lswi$EVI)),'EVI']) >= 1){
#  #  inter_evi_lswi$EVI_inter2[i] <-inter_evi_lswi$EVI_inter[i]
#  #}
#  #else 
#  if(inter_evi_lswi$DOY[i]==130){
#    if(length(inter_evi_lswi[which(inter_evi_lswi$Index == inter_evi_lswi$Index[i]
#                                       & inter_evi_lswi$DOY>115 
#                                       & inter_evi_lswi$DOY<145 
#                                       & is.finite(inter_evi_lswi$EVI)),'EVI']) >= 1){
#      inter_evi_lswi$EVI_inter2[i]<-inter_evi_lswi$EVI_inter[i]
#    }else if(is.na(LC_dat[inter_evi_lswi$Index[i]])==FALSE){
#      evi_ind<-which(values(LC_dat)[inter_evi_lswi$Index] == values(LC_dat)[inter_evi_lswi$Index[i]]
#                             & is.finite(inter_evi_lswi$EVI_inter)
#                             & ((inter_evi_lswi$x-inter_evi_lswi$x[i])^2+(inter_evi_lswi$y-inter_evi_lswi$y[i])^2)^(1/2)<400) #300 or 400m seems about good
#                             #& inter_evi_lswi$x<inter_evi_lswi$x[i]+30
#                             #& inter_evi_lswi$y<inter_evi_lswi$y[i]+50
#                             #& inter_evi_lswi$x>inter_evi_lswi$x[i]-30
#                             #& inter_evi_lswi$y>inter_evi_lswi$y[i]-50)
#      evi_vals<-inter_evi_lswi$EVI_inter[evi_ind]
#      evi_x<-inter_evi_lswi$x[evi_ind]
#      evi_y<-inter_evi_lswi$y[evi_ind]
#      #inter_evi_lswi$EVI_inter2[i]<-mean(evi_vals)
#      # Take a weighted mean based on the distance of the data from the pixel
#      inter_evi_lswi$EVI_inter2[i]<-weighted.mean(evi_vals, weight=1/(((evi_x-inter_evi_lswi$x[i])^2 + (evi_y-inter_evi_lswi$y[i])^2)^(1/2)))
#    }
#    #if (length(x.stk)==0){
#    #  x.stk <- inter_evi_lswi$x[i] 
#    #  y.stk <- inter_evi_lswi$y[i]
#    #  Date.stk <- inter_evi_lswi$DOY[i]
#    #  evi.stk <- inter_evi_lswi$EVI_inter[i]
#    #  evi2.stk <- inter_evi_lswi$EVI_inter2[i]
#    #  Index.stk<-inter_evi_lswi$Index[i]
#    #} else {
#    #  x.stk <- append(x.stk, inter_evi_lswi$x[i])
#    #  y.stk <- append(y.stk, inter_evi_lswi$y[i])
#    #  Date.stk <- append(Date.stk, inter_evi_lswi$DOY[i])
#    #  evi.stk <- append(evi.stk, inter_evi_lswi$EVI_inter[i])
#    #  evi2.stk <- append(evi2.stk, inter_evi_lswi$EVI_inter2[i])
#    #  Index.stk <-append(Index.stk,inter_evi_lswi$Index[i])
#    #  LC.stk <- append(LC.stk,LC_dat[inter_evi_lswi$Index[i]])
#    #}
#    print(round(inter_evi_lswi$Index[i], 1))
#  }
#}

#Index.stk<-NULL
#x.stk<-NULL
#y.stk<-NULL
#Date.stk<-NULL
#evi.stk<-NULL
#evi2.stk<-NULL
#LC.stk <- NULL

#for (I in 1:length(inter_evi_lswi$Index)){
#  if (130 == inter_evi_lswi$DOY[I]){ #} & inter_evi_lswi$DOY[I] < 150){ #this makes it so you only get the first day
#    if (length(x.stk)==0){
#      x.stk <- inter_evi_lswi$x[I] 
#      y.stk <- inter_evi_lswi$y[I]
#      Date.stk <- inter_evi_lswi$DOY[I]
#      evi.stk <- inter_evi_lswi$EVI_inter[I]
#      evi2.stk <- inter_evi_lswi$EVI_inter2[I]
#      Index.stk<-inter_evi_lswi$Index[I]
#      LC.stk<-LC_dat[inter_evi_lswi$Index[I]]
#    } else {
#      x.stk <- append(x.stk, inter_evi_lswi$x[I])
#      y.stk <- append(y.stk, inter_evi_lswi$y[I])
#      Date.stk <- append(Date.stk, inter_evi_lswi$DOY[I])
#      evi.stk <- append(evi.stk, inter_evi_lswi$EVI_inter[I])
#      evi2.stk <- append(evi2.stk, inter_evi_lswi$EVI_inter2[I])
#      Index.stk <-append(Index.stk,inter_evi_lswi$Index[I])
#      LC.stk <- append(LC.stk,LC_dat[inter_evi_lswi$Index[I]])
#    }
#    print(inter_evi_lswi$Index[I])
#  }
#}

#All.data.evi <- data.frame(
#  Index = Index.stk[1:length(LC.stk)],
#  x = x.stk[1:length(LC.stk)], 
#  y = y.stk[1:length(LC.stk)],
#  EVI_inter =evi.stk[1:length(LC.stk)],
#  EVI_inter2 =evi2.stk[1:length(LC.stk)],
#  LC = LC.stk[1:length(LC.stk)],
#  stringsAsFactors = FALSE
#)

#All.data.evi$EVI_inter2[which(is.na(All.data.evi$EVI_inter2))]<-All.data.evi$EVI_inter[which(is.na(All.data.evi$EVI_inter2))]
#All.data.evi$EVI_inter2[which(All.data.evi$EVI_inter2<0)]<-All.data.evi$EVI_inter[which(All.data.evi$EVI_inter2<0)]

#sincrs <- '+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'

#s <- SpatialPoints(cbind(All.data.evi$x, All.data.evi$y), proj4string=CRS(sincrs))

#lonlat <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 

#coords <- spTransform(s, lonlat)

#EVI_xy_data<-cbind(as.data.frame(coords), All.data.evi$EVI_inter, All.data.evi$EVI_inter2, All.data.evi$Index)

#library("ggplot2")
#ggplot(EVI_xy_data,                       # Draw ggplot2 plot
#       aes(x = coords.x1, 
#           y = coords.x2,
#           width=3.8*10^(-4),
#           height=2.75*10^(-4))) + geom_tile(aes(fill=All.data.evi$EVI_inter)) + ggtitle('Borden Interpolated EVI (DOY = 130)') +coord_equal()

#ggplot(EVI_xy_data,                       # Draw ggplot2 plot
#       aes(x = coords.x1, 
#           y = coords.x2,
#           width=3.8*10^(-4),
#           height=2.75*10^(-4))) + geom_tile(aes(fill=All.data.evi$EVI_inter2)) + ggtitle('Borden Interpolated EVI V2 (DOY = 130), 400') +coord_equal()

#ggplot(EVI_xy_data,                       # Draw ggplot2 plot
#       aes(x = coords.x1, 
#           y = coords.x2,
#           width=3.8*10^(-4),
#           height=2.75*10^(-4))) + geom_tile(aes(fill=All.data.evi$LC)) + ggtitle('Borden Land Cover (DOY = 130)') +coord_equal()

# Interpolate daily LSWI values for each pixel
inter_evi_lswi$LSWI_inter <- NA

for(i in unique(inter_evi_lswi$Index)){
  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$LSWI)),'LSWI']) > 3){
    pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
    spl <- with(pix[!is.na(pix$LSWI),],smooth.spline(DOY,LSWI, spar = .25)) 
    inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- predict(spl, c(1:365))$y
  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- NA}
  print(round(i/npixel*100, 1))
}

#inter_evi_lswi$LSWI_inter2 <- NA
#for(i in 1:length(inter_evi_lswi$Index)){
#  if(inter_evi_lswi$DOY[i]==130){
#    if(length(inter_evi_lswi[which(inter_evi_lswi$Index == inter_evi_lswi$Index[i]
#                                   & inter_evi_lswi$DOY>115 
#                                   & inter_evi_lswi$DOY<145 
#                                   & is.finite(inter_evi_lswi$LSWI)),'LSWI']) >= 1){
#      inter_evi_lswi$LSWI_inter2[i]<-inter_evi_lswi$LSWI_inter[i]
#    }else if(is.na(LC_dat[inter_evi_lswi$Index[i]])==FALSE){
#      lswi_vals<-inter_evi_lswi$LSWI_inter[which(values(LC_dat) == values(LC_dat)[inter_evi_lswi$Index[i]]
#                                               & is.finite(inter_evi_lswi$LSWI_inter)
#                                               & inter_evi_lswi$x<inter_evi_lswi$x[i]+200 
#                                               & inter_evi_lswi$y<inter_evi_lswi$y[i]+200
#                                               & inter_evi_lswi$x>inter_evi_lswi$x[i]-200
#                                               & inter_evi_lswi$y>inter_evi_lswi$y[i]-200)]
#      inter_evi_lswi$LSWI_inter2[i]<-mean(lswi_vals)
#    }
#  }
#  print(round(inter_evi_lswi$Index[i], 1))
#}


#lswi.stk<-NULL
#lswi2.stk<-NULL

#for (I in 1:length(inter_evi_lswi$Index)){
#  if (130 == inter_evi_lswi$DOY[I]){ #} & inter_evi_lswi$DOY[I] < 150){ #this makes it so you only get the first day
#    if (length(lswi.stk)==0){
#      lswi.stk <- inter_evi_lswi$LSWI_inter[I]
#      lswi2.stk <- inter_evi_lswi$LSWI_inter2[I]
#    } else {
#      lswi.stk <- append(lswi.stk, inter_evi_lswi$LSWI_inter[I])
#      lswi2.stk <- append(lswi2.stk, inter_evi_lswi$LSWI_inter2[I])
#    }
#  }
#}

#All.data.evi <- data.frame(
#  Index = Index.stk[1:length(LC.stk)],
#  x = x.stk[1:length(LC.stk)], 
#  y = y.stk[1:length(LC.stk)],
#  EVI_inter =evi.stk[1:length(LC.stk)],
#  EVI_inter2 =evi2.stk[1:length(LC.stk)],
#  LSWI_inter =lswi.stk[1:length(LC.stk)],
#  LSWI_inter2 =lswi2.stk[1:length(LC.stk)],
#  LC = LC.stk[1:length(LC.stk)],
#  stringsAsFactors = FALSE
#)

#EVI_xy_data<-cbind(as.data.frame(coords), All.data.evi$EVI_inter, All.data.evi$EVI_inter2, All.data.evi$LSWI_inter, All.data.evi$LSWI_inter2, All.data.evi$Index)

#ggplot(EVI_xy_data,                       # Draw ggplot2 plot
#       aes(x = coords.x1, 
#           y = coords.x2,
#           width=3.8*10^(-4),
#           height=2.75*10^(-4))) + geom_tile(aes(fill=All.data.evi$LSWI_inter)) + ggtitle('Borden Interpolated LSWI (DOY = 130)') +coord_equal()

#ggplot(EVI_xy_data,                       # Draw ggplot2 plot
#       aes(x = coords.x1, 
#           y = coords.x2,
#           width=3.8*10^(-4),
#           height=2.75*10^(-4))) + geom_tile(aes(fill=All.data.evi$LSWI_inter2)) + ggtitle('Borden Interpolated LSWI V2 (DOY = 130)') +coord_equal()


#inter_evi_lswi$EVI_inter[is.na(inter_evi_lswi$EVI_inter2)==FALSE]<-inter_evi_lswi$EVI_inter2[is.na(inter_evi_lswi$EVI_inter2)==FALSE]
#inter_evi_lswi$LSWI_inter[is.na(inter_evi_lswi$LSWI_inter2)==FALSE]<-inter_evi_lswi$LSWI_inter2[is.na(inter_evi_lswi$LSWI_inter2)==FALSE]

#inter_evi_lswi %>% select(-EVI_inter2)
#inter_evi_lswi %>% select(-LSWI_inter2)

#inter_evi_lswi

# Write Table
write.table(inter_evi_lswi,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden/adjusted_evi_lswi_interpolated_ls7and8.csv',row.names = F,
            sep=',')
