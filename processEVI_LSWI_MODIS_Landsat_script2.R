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

#MODIS:
LC_dat<-raster("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD_V061_500m_2018/LandCover/MODIS_V061_LC_TPD_500m_2018.tif")
#SOLRIS:
#LC_dat<-raster("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden_SOLRIS/LandCover/LC_Borden.tif")
## Calculate EVI and LSWI indices for Landsat images that have been cropped to the study domain

#LANDSAT 8 files
setwd('E:/Research/UrbanVPRM/dataverse_files/TPD/landsat/landsat8') # landsat data in /urbanVPRM_30m/driver_data/landsat/
ls8 <- list.files()
for (i in 1:length(ls8)){
  file <- stack(ls8[i])
  ## apply scale factors for reflectance data in the bands needed for EVI/LSWI calculation
  file[[2]] <- file[[2]] * 0.0001
  file[[4]] <- file[[4]] * 0.0001
  file[[5]] <- file[[5]] * 0.0001
  file[[6]] <- file[[6]] * 0.0001
  file[[7]] <- file[[7]] * 0.0001
  clear_code <- c(322, 386, 834, 898, 1346)
  bad_pixels <- which(!(values(file[[11]]) %in% clear_code))
  values(file[[2]])[bad_pixels] <- NA
  values(file[[4]])[bad_pixels] <- NA
  values(file[[5]])[bad_pixels] <- NA
  values(file[[6]])[bad_pixels] <- NA
  values(file[[7]])[bad_pixels] <- NA
  ## calculate EVI
  file[[13]] <- 2.5 * ((file[[5]] - file[[4]]) / (file[[5]] + 6 * file[[4]] - 7.5 * file[[2]] + 1))
  ## calculate LSWI
  #file[[14]] <- (file[[5]] - file[[6]]) / (file[[5]] + file[[6]])
  file[[14]] <- (file[[5]] - file[[7]]) / (file[[5]] + file[[7]])
  
  #Reproject/resample to MODIS resolution
  file <- resample(projectRaster(file,crs=crs(LC_dat)),LC_dat)
  
  EVI.dt = as.data.table(as.data.frame(file[[13]], xy=T))
  EVI.dt = cbind(1:ncell(file[[13]]), EVI.dt)
  setnames(EVI.dt,c("Index","x","y", "EVI"))
  setkey(EVI.dt,Index,x,y)
  ## LSWI
  LSWI.dt = as.data.table(as.data.frame(file[[14]], xy=T))
  LSWI.dt = cbind(1:ncell(file[[14]]), LSWI.dt)
  LSWI.dt$DOY = yday(ymd(paste0('2018',substr(ls8[i],12,15))))
  setnames(LSWI.dt,c("Index","x","y", "LSWI","DOY"))
  setkey(LSWI.dt,Index,x,y)
  ## save a data.table for each image
  assign(paste0('EVI_LSWI',i,'_8.dt'), merge(EVI.dt,LSWI.dt,by=c("Index","x","y")))
}

# landsat 7
setwd('E:/Research/UrbanVPRM/dataverse_files/TPD/landsat/landsat7')
ls7 <- list.files()
for (i in 1:length(ls7)){
  ## read in raster data
  file <- stack(ls7[i])
  ## apply scale factors for reflectance data in the bands needed for EVI/LSWI calculation
  file[[1]] <- file[[1]] * 0.0001
  file[[3]] <- file[[3]] * 0.0001
  file[[4]] <- file[[4]] * 0.0001
  file[[5]] <- file[[5]] * 0.0001
  file[[7]] <- file[[7]] * 0.0001
  ## Only keep clear observations (QA band 10 codes 66 or 130)
  clear_code <- c(66, 130)
  bad_pixels <- which(!(values(file[[10]]) %in% clear_code))
  values(file[[1]])[bad_pixels] <- NA
  values(file[[3]])[bad_pixels] <- NA
  values(file[[4]])[bad_pixels] <- NA
  values(file[[5]])[bad_pixels] <- NA
  values(file[[7]])[bad_pixels] <- NA
  ## calculate EVI
  file[[12]] <- 2.5 * ((file[[4]] - file[[3]]) / (file[[4]] + 6 * file[[3]] - 7.5 * file[[1]] + 1))
  ## calculate LSWI
  file[[13]] <- (file[[4]] - file[[7]]) / (file[[4]] + file[[7]])
  
  #Reproject/resample to MODIS resolution
  file <- resample(projectRaster(file,crs=crs(LC_dat)),LC_dat)
  
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
  LSWI.dt$DOY = yday(ymd(paste0('2018',substr(ls7[i],12,15))))
  setnames(LSWI.dt,c("Index","x","y", "LSWI","DOY"))
  setkey(LSWI.dt,Index,x,y)
  ## save a data.table for each image
  assign(paste0('EVI_LSWI',i,'_7.dt'), merge(EVI.dt,LSWI.dt,by=c("Index","x","y")))
}

## merge daily files into one for all of 2018
## MAKE SURE TO CHANGE FOR EACH SITE (some locations have more/fewer overpasses)
EVI_LSWI_ls.dt <- rbind(EVI_LSWI1_7.dt,EVI_LSWI2_7.dt,EVI_LSWI3_7.dt,EVI_LSWI4_7.dt,EVI_LSWI5_7.dt,EVI_LSWI6_7.dt,
                     EVI_LSWI7_7.dt,EVI_LSWI8_7.dt,EVI_LSWI9_7.dt,EVI_LSWI10_7.dt,EVI_LSWI11_7.dt, EVI_LSWI12_7.dt,
                     EVI_LSWI1_8.dt,EVI_LSWI2_8.dt,EVI_LSWI3_8.dt,EVI_LSWI4_8.dt,EVI_LSWI5_8.dt,EVI_LSWI6_8.dt,
                     EVI_LSWI7_8.dt,EVI_LSWI8_8.dt,EVI_LSWI9_8.dt,EVI_LSWI10_8.dt,EVI_LSWI11_8.dt,EVI_LSWI12_8.dt)


## order by DOY
EVI_LSWI_ls.dt <- EVI_LSWI_ls.dt[order(EVI_LSWI_ls.dt$DOY),]

## convert to dataframe
EVI_LSWI_ls.df <- as.data.frame(EVI_LSWI_ls.dt)

## There are a few erroneous values with EVI = 2.5. These are omitted from the dataset
EVI_LSWI_ls.df[which(EVI_LSWI_ls.df$EVI > 1),'EVI'] <- NA
EVI_LSWI_ls.df[which(EVI_LSWI_ls.df$EVI < -1),'EVI'] <- NA

EVI_LSWI_ls.df[which(EVI_LSWI_ls.df$LSWI > 1),'LSWI'] <- NA #same for LSWI (should only have values between -1 and 1)
EVI_LSWI_ls.df[which(EVI_LSWI_ls.df$LSWI < -1),'LSWI'] <- NA

## Set up new dataframe for EVI/LSWI Interpolation
npixel <- length(unique(EVI_LSWI_ls.df$Index))
inter_evi_lswi_ls <- as.data.frame(matrix(ncol = 2, nrow = npixel*378)) #min day is 2017 doy 361, max day is 2019 doy 9. 378 days total
#inter_evi_lswi_ls <- as.data.frame(matrix(ncol = 2, nrow = npixel*365))
colnames(inter_evi_lswi_ls) <- c('Index', 'DOY')
inter_evi_lswi_ls[,1] <- rep(seq(1,npixel,1), 378)
#inter_evi_lswi_ls[,1] <- rep(seq(1,npixel,1), 365)
inter_evi_lswi_ls <- inter_evi_lswi_ls[order(inter_evi_lswi_ls[,1]),]
inter_evi_lswi_ls[,2] <- rep(seq(-3,374,1), npixel)
#inter_evi_lswi_ls[,2] <- rep(seq(1,365,1), npixel)
inter_evi_lswi_ls <- merge(inter_evi_lswi_ls, EVI_LSWI_ls.df[,c('Index', 'EVI', 'LSWI', 'DOY')], by = c('Index', 'DOY'), all = TRUE)
inter_evi_lswi_ls  <- inter_evi_lswi_ls[order(inter_evi_lswi_ls[,'Index'],inter_evi_lswi_ls[,'DOY']),]

# add coordinates
x <- as.data.frame(tapply(EVI_LSWI_ls.df$x, EVI_LSWI_ls.df$Index, mean, na.rm = T))
y <- as.data.frame(tapply(EVI_LSWI_ls.df$y, EVI_LSWI_ls.df$Index, mean, na.rm = T))
xy <- cbind(as.numeric(rownames(x)), x, y)
colnames(xy) <- c('Index', 'x', 'y')

inter_evi_lswi_ls <- merge(inter_evi_lswi_ls, xy, by = 'Index', all = TRUE)
inter_evi_lswi_ls <- inter_evi_lswi_ls[,c('Index', 'DOY', 'x', 'y', 'EVI', 'LSWI')]

# Interpolate daily EVI values for each pixel
inter_evi_lswi$EVI_inter <- NA

for(i in unique(inter_evi_lswi$Index)){
  #  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$EVI)),'EVI']) > 10){
  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$EVI)),'EVI']) > 3){
    pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
    spl <- with(pix[!is.na(pix$EVI),],smooth.spline(DOY,EVI, spar = .25))
    #-    inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- predict(spl, c(-3:374))$y
    inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- predict(spl, c(1:365))$y
  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- NA}
  print(round(i/npixel*100, 1))
}
  
# Interpolate daily LSWI values for each pixel
inter_evi_lswi$LSWI_inter <- NA
  
for(i in unique(inter_evi_lswi$Index)){
#    -  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$LSWI)),'LSWI']) > 10){
  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$LSWI)),'LSWI']) > 3){
    pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
    spl <- with(pix[!is.na(pix$LSWI),],smooth.spline(DOY,LSWI, spar = .25))
#-    inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- predict(spl, c(-3:374))$y
    inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- predict(spl, c(1:365))$y
  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- NA}
  print(round(i/npixel*100, 1))
}
    
# Write Table
#write.table(inter_evi_lswi,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden/adjusted_evi_lswi_interpolated_ls7and8.csv',row.names = F,
#            sep=',')
+write.table(inter_evi_lswi,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD/evi_lswi_interpolated_ls7and8.csv',row.names = F,
+            sep=',')
    



#MODIS reflectance files
setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/MODIS_reflectance/MODIS_V061_GTA_AppEEARS') # landsat data in /urbanVPRM_30m/driver_data/landsat/
mod_files <- list.files(pattern = 'MOD09A1.061_sur_refl_b01')
yr<-2018


#QC values where red,NIR, and blue pass the quality test:
EVI_qc<-c(1073741824,1073954817,1075838976,1075838977,1075838979,1076051969,
          1076625411,1077149697,1077411843,1119879171,1121976323,1123287043,
          1123549187,1128267777,1130364929,1130364931,1131151363,1131675649,
          1131937795,1132462083,1134559235,1135345667,1135869955,1136132099,
          1811939331,1814036483,1814822915,1815347203,1815609347,1858076675,
          1860173827,1861484547,1861746691,1866465283,1868562435,1869873155,
          1870135299,1870659587,1872756739,1874067459,1874329603,1946157057,
          1946370049,1948254209,1948254211,1948467201,1949040643,1949564929,
          1949827075,1992294403,1994391555,1995702275,1995964419,2000683009,
          2002780161,2002780163,2003566595,2004090881,2004303873,2004353027,
          2004877315,2006974467,2007760899,2008285187,2008547331,2013265923,
          2013478915,2015363075,2015576067,2016149507,2016673795,2016886787,
          2016935939,2059403267,2061500419,2062811139,2063073283,2067791875,
          2069889027,2070675459,2071199747,2071461891,2071986179,2074083331,
          2074869763,2075394051,2075656195)
#QC values where NIR and SWIR pass the quality test:
LSWI_qc<-c(1073741824,1073741877,1073954817,1073954869,1075838976,1075838977,
           1075838979,1075839029,1076051969,1076052021,1076625411,1077149697,
           1077149749,1077362741,1077411843,1077411895,1119879171,1119879223,
           1121976323,1121976375,1123287043,1123287095,1123549187,1128267777,
           1128267829,1128480821,1130364929,1130364931,1130364981,1131151363,
           1131675649,1131675701,1131937795,1131937847,1132462083,1132462135,
           1132675127,1134559235,1134559287,1135345667,1135345719,1135869955,
           1135870007,1136132099,1136132151)
#ok_qc<-c(1073741824,1073954817,1075838976,1075838977,1075838979,1076051969,
#         1076625411,1077149697,1077411843,1119879171,1121976323,1123287043,
#         1123549187,1128267777,1130364929,1130364931,1131151363,1131675649,
#         1131937795,1132462083,1134559235,1135345667,1135869955,1136132099)


for (i in 1:length(mod_files)){
  ## read in raster stack
  mod_file_red <- crop(stack(mod_files[i]),LC_dat)
  mod_file_NIR <-crop(stack(paste(substr(mod_files[i],1,23),"2",substr(mod_files[i],25,47),sep = "")),LC_dat)
  mod_file_blue <-crop(stack(paste(substr(mod_files[i],1,23),"3",substr(mod_files[i],25,47),sep = "")),LC_dat)
  mod_file_SWIR <-crop(stack(paste(substr(mod_files[i],1,23),"7",substr(mod_files[i],25,47),sep = "")),LC_dat)
  mod_file_QC <- crop(stack(paste(substr(mod_files[i],1,21),"qc_500m",substr(mod_files[i],25,47),sep = "")),LC_dat)
  
  file <- stack(mod_file_red,mod_file_NIR,mod_file_blue,mod_file_SWIR,mod_file_QC)
  # For the V061 data it appears the scale factors have already been applied to the data (before)
  ## apply scale factors for reflectance data in the bands needed for EVI/LSWI calculation
  #file[[1]] <- file[[1]] * 0.0001
  #file[[2]] <- file[[2]] * 0.0001
  #file[[3]] <- file[[3]] * 0.0001
  #file[[4]] <- file[[4]] * 0.0001
  
  
  ## only keep clear observations determined from CFmask (pixel QA band 11 codes 322, 386, 834, 898, or 1346)
  #clear_code <- c(322, 386, 834, 898, 1346)
  EVI_bad_pixels <- which(!(values(file[[5]])) %in% EVI_qc) 
  LSWI_bad_pixels <- which(!(values(file[[5]])) %in% LSWI_qc)
  file[[1]][EVI_bad_pixels]<-NA
  #file[[2]][EVI_bad_pixels]<-NA
  file[[3]][EVI_bad_pixels]<-NA
  
  #file[[2]][LSWI_bad_pixels]<-NA
  file[[4]][LSWI_bad_pixels]<-NA
  #values(file[[2]])[bad_pixels] <- NA
  #values(file[[4]])[bad_pixels] <- NA
  #values(file[[5]])[bad_pixels] <- NA
  #values(file[[6]])[bad_pixels] <- NA
  ### calculate EVI
  file[[6]] <- 2.5 * ((file[[2]] - file[[1]]) / (file[[2]] + 6 * file[[1]] - 7.5 * file[[3]] + 1))
  #file[[5]][abs(file[[5]])>1]<-NA
  ## calculate LSWI
  file[[7]] <- (file[[2]] - file[[4]]) / (file[[2]] + file[[4]])
  #Replace eroneous values with NA
  #file[[6]][abs(file[[6]])>1]<-NA
  ## convert raster data to data.table and assign Day of Year values
  ## EVI
  EVI.dt = as.data.table(as.data.frame(file[[6]], xy=T))
  EVI.dt = cbind(1:ncell(file[[6]]), EVI.dt)
  setnames(EVI.dt,c("Index","x","y", "EVI"))
  setkey(EVI.dt,Index,x,y)
  ## LSWI
  LSWI.dt = as.data.table(as.data.frame(file[[7]], xy=T))
  LSWI.dt = cbind(1:ncell(file[[7]]), LSWI.dt)
  #LSWI.dt$DOY = yday(ymd(paste0('2018',substr((names(file))[1],3,6))))
  #The line above was trying to use the first column of the file instead of the
  # file name (replacement bellow uses file name)
  #TPD:
  #LSWI.dt$DOY = yday(ymd(paste0('2018',substr(ls8[i],12,15))))
  #MODIS:
  if (as.numeric((substr(mod_files[i],29,32)))<yr){
    LSWI.dt$DOY = as.numeric((substr(mod_files[i],33,35)))-365+1
  }else if (as.numeric((substr(mod_files[i],29,32)))>yr){
    LSWI.dt$DOY = as.numeric((substr(mod_files[i],33,35)))+365
  }else{
    LSWI.dt$DOY = as.numeric((substr(mod_files[i],33,35)))
  }
  setnames(LSWI.dt,c("Index","x","y", "LSWI","DOY"))
  setkey(LSWI.dt,Index,x,y)
  ## save a data.table for each image
  assign(paste0('EVI_LSWI',i,'.dt'), merge(EVI.dt,LSWI.dt,by=c("Index","x","y")))
}

EVI_LSWI_mod.dt <- rbind(EVI_LSWI1.dt,EVI_LSWI2.dt,EVI_LSWI3.dt,EVI_LSWI4.dt,EVI_LSWI5.dt,EVI_LSWI6.dt,
                     EVI_LSWI7.dt,EVI_LSWI8.dt,EVI_LSWI9.dt,EVI_LSWI10.dt,EVI_LSWI11.dt, EVI_LSWI12.dt,
                     EVI_LSWI13.dt,EVI_LSWI14.dt,EVI_LSWI15.dt,EVI_LSWI16.dt,EVI_LSWI17.dt,EVI_LSWI18.dt,
                     EVI_LSWI19.dt,EVI_LSWI20.dt,EVI_LSWI21.dt,EVI_LSWI22.dt,EVI_LSWI23.dt,EVI_LSWI24.dt,
                     EVI_LSWI25.dt,EVI_LSWI26.dt,EVI_LSWI27.dt,EVI_LSWI28.dt,EVI_LSWI29.dt,EVI_LSWI30.dt,
                     EVI_LSWI31.dt,EVI_LSWI32.dt,EVI_LSWI33.dt,EVI_LSWI34.dt,EVI_LSWI35.dt,EVI_LSWI36.dt,
                     EVI_LSWI37.dt,EVI_LSWI38.dt,EVI_LSWI39.dt,EVI_LSWI40.dt,EVI_LSWI41.dt,EVI_LSWI42.dt,
                     EVI_LSWI43.dt,EVI_LSWI44.dt,EVI_LSWI45.dt,EVI_LSWI46.dt,EVI_LSWI47.dt,EVI_LSWI48.dt,
                     EVI_LSWI49.dt) 
                #46 files for 2018, plus one from end of 2017, and two from beginning of 2019

## order by DOY
EVI_LSWI_mod.dt <- EVI_LSWI_mod.dt[order(EVI_LSWI_mod.dt$DOY),]

## convert to dataframe
EVI_LSWI_mod.df <- as.data.frame(EVI_LSWI_mod.dt)

## There are a few erroneous values with EVI >1 or EVI < -1 (e.g.EVI = 2.5). These are omitted from the dataset 
EVI_LSWI_mod.df[which(EVI_LSWI_mod.df$EVI > 1),'EVI'] <- NA
EVI_LSWI_mod.df[which(EVI_LSWI_mod.df$EVI < -1),'EVI'] <- NA

EVI_LSWI_mod.df[which(EVI_LSWI_mod.df$LSWI > 1),'LSWI'] <- NA #same for LSWI (should only have values between -1 and 1)
EVI_LSWI_mod.df[which(EVI_LSWI_mod.df$LSWI < -1),'LSWI'] <- NA

## Set up new dataframe for EVI/LSWI Interpolation
npixel <- length(unique(EVI_LSWI_mod.df$Index))
inter_evi_lswi_mod <- as.data.frame(matrix(ncol = 2, nrow = npixel*378)) #min day is 2017 doy 361, max day is 2019 doy 9. 378 days total
colnames(inter_evi_lswi_mod) <- c('Index', 'DOY')
inter_evi_lswi_mod[,1] <- rep(seq(1,npixel,1), 378)
inter_evi_lswi_mod <- inter_evi_lswi_mod[order(inter_evi_lswi_mod[,1]),]
inter_evi_lswi_mod[,2] <- rep(seq(-3,374,1), npixel)
inter_evi_lswi_mod <- merge(inter_evi_lswi_mod, EVI_LSWI_mod.df[,c('Index', 'EVI', 'LSWI', 'DOY')], by = c('Index', 'DOY'), all = TRUE)
inter_evi_lswi_mod  <- inter_evi_lswi_mod[order(inter_evi_lswi_mod[,'Index'],inter_evi_lswi_mod[,'DOY']),]

# add coordinates
x <- as.data.frame(tapply(EVI_LSWI.df$x, EVI_LSWI.df$Index, mean, na.rm = T))
y <- as.data.frame(tapply(EVI_LSWI.df$y, EVI_LSWI.df$Index, mean, na.rm = T))
xy <- cbind(as.numeric(rownames(x)), x, y)
colnames(xy) <- c('Index', 'x', 'y')

inter_evi_lswi_mod <- merge(inter_evi_lswi_mod, xy, by = 'Index', all = TRUE)
inter_evi_lswi_mod <- inter_evi_lswi_mod[,c('Index', 'DOY', 'x', 'y', 'EVI', 'LSWI')]

#If there are no modis values, replace with landsat values
inter_evi_lswi_mod$EVI[is.na(inter_evi_lswi_mod$EVI) & is.na(inter_evi_lswi_ls$EVI)==FALSE]<-inter_evi_lswi_ls$EVI[is.na(inter_evi_lswi_mod$EVI) & is.na(inter_evi_lswi_ls$EVI)==FALSE]
inter_evi_lswi_mod$LSWI[is.na(inter_evi_lswi_mod$LSWI) & is.na(inter_evi_lswi_ls$LSWI)==FALSE]<-inter_evi_lswi_ls$LSWI[is.na(inter_evi_lswi_mod$LSWI) & is.na(inter_evi_lswi_ls$LSWI)==FALSE]

inter_evi_lswi<-inter_evi_lswi_mod
# Interpolate daily EVI values for each pixel
inter_evi_lswi$EVI_inter <- NA

for(i in unique(inter_evi_lswi$Index)){
  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$EVI)),'EVI']) > 10){
    pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
    spl <- with(pix[!is.na(pix$EVI),],smooth.spline(DOY,EVI, spar = .25)) 
    inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- predict(spl, c(-3:374))$y
  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- NA}
  print(round(i/npixel*100, 1))
}


# Interpolate daily LSWI values for each pixel
inter_evi_lswi$LSWI_inter <- NA

for(i in unique(inter_evi_lswi$Index)){
  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$LSWI)),'LSWI']) > 10){
    pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
    spl <- with(pix[!is.na(pix$LSWI),],smooth.spline(DOY,LSWI, spar = .25)) 
    inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- predict(spl, c(-3:374))$y
  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- NA}
  print(round(i/npixel*100, 1))
}

inter_evi_lswi$LSWI_inter_linear<-NA

#inter_evi_lswi$LSWI_inter_no_smooth<-inter_evi_lswi$LSWI
library(zoo)

for(i in unique(inter_evi_lswi$Index)){
  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$LSWI)),'LSWI']) > 10){
    inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter_linear'] <- na.approx(inter_evi_lswi$LSWI[inter_evi_lswi$Index==i],na.rm=FALSE)
  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter_linear'] <- NA}
  print(round(i/npixel*100, 1))
}

#inter_evi_lswi$LSWI_inter_linear[inter_evi_lswi$Index==i]<-na.approx(inter_evi_lswi$LSWI_inter_linear[inter_evi_lswi$Index==i])

#i<-11026
#if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$LSWI)),'LSWI']) > 10){
#  pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
#  #spl <- with(pix[!is.na(pix$LSWI),],smooth.spline(DOY,LSWI, spar =0.4)) 
#  spl <-with(pix[!is.na(pix$LSWI),],approx(DOY,y=LSWI)) 
#  inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter_40'] <- predict(approx, c(-3:374))$y
#} else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter_40'] <- NA}

#THERE ARE STILL SOME NA VALUES IN THE LINEAR INTERPOLATED AT THE BEGINING/END OF THE YEAR (should mostly be filtered out in VPRM code though)
inter_evi_lswi_test<-inter_evi_lswi

inter_evi_lswi.dt<-as.data.table(inter_evi_lswi_test)
inter_evi_lswi.dt<-inter_evi_lswi.dt[inter_evi_lswi.dt$DOY>0 & inter_evi_lswi.dt$DOY<366] #select only data that falls within the year
inter_evi_lswi.df<-as.data.frame(inter_evi_lswi.dt)#convert back to dataframe


#uncomment line below to save linearly ionterpolated LSWI
inter_evi_lswi.df$LSWI_inter<-inter_evi_lswi.df$LSWI_inter_linear
inter_evi_lswi.df$LSWI_inter_linear<-NULL

inter_evi_lswi.df$LSWI_inter[inter_evi_lswi.df$LSWI_inter<min(inter_evi_lswi.df$LSWI,na.rm=TRUE) & !is.na(inter_evi_lswi.df$LSWI_inter)]<-min(inter_evi_lswi.df$LSWI,na.rm=TRUE)
inter_evi_lswi.df$LSWI_inter[inter_evi_lswi.df$LSWI_inter>max(inter_evi_lswi.df$LSWI,na.rm=TRUE) & !is.na(inter_evi_lswi.df$LSWI_inter)]<-max(inter_evi_lswi.df$LSWI,na.rm=TRUE)



#stp<-inter_evi_lswi.df$x[366]-inter_evi_lswi.df$x[1]
#das<-unique(inter_evi_lswi.df$DOY[inter_evi_lswi.df$LSWI_inter < min(inter_evi_lswi.df$LSWI,na.rm=TRUE) | inter_evi_lswi.df$LSWI_inter > max(inter_evi_lswi.df$LSWI,na.rm=TRUE)])

#inter_evi_lswi.df$LSWI_test<-inter_evi_lswi.df$LSWI_inter

#for (d in das) {
#  idx<-which((inter_evi_lswi.df$LSWI_inter< min(inter_evi_lswi.df$LSWI,na.rm=TRUE) | inter_evi_lswi.df$LSWI_inter> max(inter_evi_lswi.df$LSWI,na.rm=TRUE)) & inter_evi_lswi.df$DOY==d)
#  for (i in idx){
#    id<-which(inter_evi_lswi.df$x <= inter_evi_lswi.df$x[i]+stp & inter_evi_lswi.df$x >= inter_evi_lswi.df$x[i]-stp & inter_evi_lswi.df$y <= inter_evi_lswi.df$y[i]+stp & inter_evi_lswi.df$y[i] >= inter_evi_lswi.df$y[i]-stp & inter_evi_lswi.df$DOY==d)
#    id<-id[which(inter_evi_lswi.df$LSWI_inter[id]>min(inter_evi_lswi.df$LSWI,na.rm=TRUE) & inter_evi_lswi.df$LSWI_inter[id]<max(inter_evi_lswi.df$LSWI,na.rm=TRUE))]
#    if(length(id)<2){#if there is 1 or less pixel that is within the valid range
#      inter_evi_lswi.df$LSWI_test[i]<-mean(inter_evi_lswi.df$LSWI_inter[inter_evi_lswi.df$DOY==d & inter_evi_lswi.df$LSWI_inter>min(inter_evi_lswi.df$LSWI,na.rm=TRUE &inter_evi_lswi.df$LSWI_inter<max(inter_evi_lswi.df$LSWI,na.rm=TRUE))],na.rm=TRUE) #take the mean of the entire scene
#      print(paste(d,i,'scene',sep=' '))
#    }else{
#      inter_evi_lswi.df$LSWI_test[i]<-mean(inter_evi_lswi.df$LSWI_inter[id],na.rm=TRUE)
#    }
#    #dm$sw_test[i]<-mean(dm$swrad[id],na.rm=TRUE)
#    #print(c(h,i,id))
#  }
#  print(d)
#  #for (i in length(dm$x[idx])){
#  #  print(paste0("x: "+str(dm$x[idx][i])+"surrounding: "+str(vals[i])))
#  #}
#  
#}

##inter_evi_lswi[inter_evi_lswi$DOY>0 & inter_evi_lswi<366]

write.table(inter_evi_lswi.df,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD_V061_500m_2018/adjusted_evi_lswi_interpolated_landsat_modis_v061_qc_filtered_LSWI_filtered.csv',row.names = F,
            sep=',')







##create a raster brick and set the number of columns, rows, layers, and extent 
## NEED TO CHANGE DEPENDING ON SITE
#EVI_raster <- brick(ncol=16, nrow=16, xmn= xmin(LC_dat), xmx= xmax(LC_dat), ymn= ymin(LC_dat), ymx= ymax(LC_dat), nl=365)
###check the resolution is 1/240
#res(EVI_raster)
###check the number of cells is 256 for the GTA
#ncell(EVI_raster)
#names(EVI_raster)<-paste('DOY',1:365,sep="") #name the layers

#LSWI_raster <- brick(ncol=16, nrow=16, xmn= xmin(LC_dat), xmx= xmax(LC_dat), ymn= ymin(LC_dat), ymx= ymax(LC_dat), nl=365)
#names(LSWI_raster)<-paste('DOY',1:365,sep="") #name the layers

#
#for (i in 1:length(mod_files)){
#  ## read in raster data
#  mod_file_red <- stack(mod_files[i])
#  mod_file_NIR <-stack(paste(substr(mod_files[i],1,23),"2",substr(mod_files[i],25,47),sep = ""))
#  mod_file_blue <-stack(paste(substr(mod_files[i],1,23),"3",substr(mod_files[i],25,47),sep = ""))
#  mod_file_SWIR <-stack(paste(substr(mod_files[i],1,23),"7",substr(mod_files[i],25,47),sep = ""))
#  file_red<-raster::crop(mod_file_red, EVI_raster,method = 'ngb')
#  file_NIR<-raster::crop(mod_file_NIR, EVI_raster,method = 'ngb')
#  file_blue<-raster::crop(mod_file_blue, EVI_raster,method = 'ngb')
#  file_SWIR<-raster::crop(mod_file_SWIR, EVI_raster,method = 'ngb')
#  
#  file_red <- file_red * 0.0001 #red
#  file_NIR <- file_NIR * 0.0001 #NIR
#  file_blue <- file_blue * 0.0001 #blue
#  file_SWIR <- file_SWIR * 0.0001 #SWIR
#  
#  ## Only keep clear observations (QA band 10 codes 66 or 130)
#  #clear_code <- c(16392,16400,16456,16464,16520,16528,16584,16592,16904,16912,16968,16976,17032,17040,17096,17104,24584,24592,24648,24656,24712,24720,24776,24784,25096,25104,25160,25176,25224,25232,25288,25296)
#  #bad_pixels <- which(!(values(file[[12]]) %in% clear_code))
#  #values(file[[1]])[bad_pixels] <- NA
#  #values(file[[2]])[bad_pixels] <- NA
#  #values(file[[3]])[bad_pixels] <- NA
#  #values(file[[7]])[bad_pixels] <- NA
#  ## calculate EVI
#  file_evi <- 2.5 * ((file_NIR - file_red) / (file_NIR + 6 * file_red - 7.5 * file_blue + 1))
#  ## calculate LSWI
#  file_lswi <- (file_NIR - file_SWIR) / (file_NIR + file_SWIR)
#  
#  file_evi[abs(file_evi)>1]<-NA
#  file_lswi[abs(file_lswi)>1]<-NA
#  
#  doy<-substr(mod_files[i],33,35)
#  if(doy<10){
#    DOY_layer<-paste('DOY',substr(doy,3,3),sep="")
#  }else if (doy<100){
#    DOY_layer<-paste('DOY',substr(doy,2,3),sep="")
#  }else{
#    DOY_layer<-paste('DOY',doy,sep="")
#  }
  
#  EVI_raster[[DOY_layer]]<-file_evi
#  
#  LSWI_raster[[DOY_layer]]<-file_lswi
#  print(round(i/length(mod_files)*100,1))
#}
#rm(mod_file_red,mod_file_blue,mod_file_NIR,mod_file_SWIR)

#Inter_EVI_raster <-raster()
##set the number of columns, rows, and extent NEED TO CHANGE DEPENDING ON SITE
#Inter_EVI_raster <- brick(ncol=16, nrow=16, xmn= xmin(LC_dat), xmx= xmax(LC_dat), ymn= ymin(LC_dat), ymx= ymax(LC_dat), nl=365)
#names(Inter_EVI_raster)<-paste('DOY',1:365,sep="") #name the layers

#EVI_values<-values(EVI_raster)
#EVI_length<-length(values(EVI_raster$DOY1))
#Inter_EVI_values<-values(Inter_EVI_raster)

#for(j in 1:length(values(EVI_raster$DOY1))){
#  if(length(EVI_values[j,][which(is.finite(EVI_values[j,]))]) > 10){
#    EVI<-EVI_values[j,]
#    DOY<-c(1:365)
#    
#    pix<-data.frame(DOY,EVI)
#    
#    spl<- with(pix[!is.na(pix$EVI),],smooth.spline(DOY,EVI, spar = .25))
#    Inter_EVI_values[j,]<-predict(spl, c(1:365))$y
#  } else {Inter_EVI_values[j,]<- NA}
#  print(round(j/EVI_length*100, 1))
#}

#values(Inter_EVI_raster)<-Inter_EVI_values
#Inter_EVI_raster[abs(Inter_EVI_raster)>1.5]<-NA
#Inter_EVI_raster[Inter_EVI_raster>1]<-1
#Inter_EVI_raster[-1>Inter_EVI_raster]<- -1


#Inter_LSWI_raster <-raster()
##set the number of columns, rows, and extent NEED TO CHANGE DEPENDING ON SITE
#Inter_LSWI_raster <- brick(ncol=16, nrow=16, xmn= xmin(LC_dat), xmx= xmax(LC_dat), ymn= ymin(LC_dat), ymx= ymax(LC_dat), nl=365)
#names(Inter_LSWI_raster)<-paste('DOY',1:365,sep="") #name the layers

#LSWI_values<-values(LSWI_raster)
#LSWI_length<-length(values(LSWI_raster$DOY1))
#Inter_LSWI_values<-values(Inter_LSWI_raster)

#for(i in 1:length(values(LSWI_raster$DOY1))){
#  if(length(LSWI_values[i,][which(is.finite(LSWI_values[i,]))]) > 10){
#    LSWI<-LSWI_values[i,]
#    DOY<-c(1:365)
#    
#    pix<-data.frame(DOY,LSWI)
#    
#    spl<- with(pix[!is.na(pix$LSWI),],smooth.spline(DOY,LSWI, spar = .25))
#    Inter_LSWI_values[i,]<-predict(spl, c(1:365))$y
#  } else {Inter_LSWI_values[i,]<- NA}
#  print(round(i/LSWI_length*100, 1))
#}

#values(Inter_LSWI_raster)<-Inter_LSWI_values
#Inter_LSWI_raster[abs(Inter_LSWI_raster)>1.5]<-NA
#Inter_LSWI_raster[Inter_LSWI_raster>1]<-1
#Inter_LSWI_raster[-1>Inter_LSWI_raster]<- -1

#writeRaster(Inter_LSWI_raster,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden_500m/MODIS_LSWI.tif",
#            overwrite=TRUE)




# LANDSAT 8 files
#setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden/landsat/landsat8') # landsat data in /urbanVPRM_30m/driver_data/landsat/
#ls8 <- list.files()
#for (i in 1:length(ls8)){
#  ## read in raster stack
#  file <- stack(ls8[i])
#  ## apply scale factors for reflectance data in the bands needed for EVI/LSWI calculation
#  file[[2]] <- file[[2]] * 0.0001
#  file[[4]] <- file[[4]] * 0.0001
#  file[[5]] <- file[[5]] * 0.0001
#  file[[6]] <- file[[6]] * 0.0001
#  ## only keep clear observations determined from CFmask (pixel QA band 11 codes 322, 386, 834, 898, or 1346)
#  clear_code <- c(322, 386, 834, 898, 1346)
#  bad_pixels <- which(!(values(file[[11]]) %in% clear_code))
#  values(file[[2]])[bad_pixels] <- NA
#  values(file[[4]])[bad_pixels] <- NA
#  values(file[[5]])[bad_pixels] <- NA
#  values(file[[6]])[bad_pixels] <- NA
#  ## calculate EVI
#  file[[13]] <- 2.5 * ((file[[5]] - file[[4]]) / (file[[5]] + 6 * file[[4]] - 7.5 * file[[2]] + 1))
#  ## calculate LSWI
#  file[[14]] <- (file[[5]] - file[[6]]) / (file[[5]] + file[[6]])
#  ## convert raster data to data.table and assign Day of Year values
#  ## EVI
#  EVI.dt = as.data.table(as.data.frame(file[[13]], xy=T))
#  EVI.dt = cbind(1:ncell(file[[13]]), EVI.dt)
#  setnames(EVI.dt,c("Index","x","y", "EVI"))
#  setkey(EVI.dt,Index,x,y)
#  ## LSWI
#  LSWI.dt = as.data.table(as.data.frame(file[[14]], xy=T))
#  LSWI.dt = cbind(1:ncell(file[[14]]), LSWI.dt)
#  #LSWI.dt$DOY = yday(ymd(paste0('2018',substr((names(file))[1],3,6))))
#  #The line above was trying to use the first column of the file instead of the
#  # file name (replacement bellow uses file name)
#  #TPD:
#  #LSWI.dt$DOY = yday(ymd(paste0('2018',substr(ls8[i],12,15))))
#  #Borden:
#  LSWI.dt$DOY = yday(ymd(paste0('2018',substr(ls8[i],3,6))))
#  setnames(LSWI.dt,c("Index","x","y", "LSWI","DOY"))
#  setkey(LSWI.dt,Index,x,y)
#  ## save a data.table for each image
#  assign(paste0('EVI_LSWI',i,'_8.dt'), merge(EVI.dt,LSWI.dt,by=c("Index","x","y")))
#}

## landsat 7
#setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden/landsat/landsat7')
#ls7 <- list.files()
#for (i in 1:length(ls7)){
#  ## read in raster data
#  file <- stack(ls7[i])
#  ## apply scale factors for reflectance data in the bands needed for EVI/LSWI calculation
#  file[[1]] <- file[[1]] * 0.0001
#  file[[3]] <- file[[3]] * 0.0001
#  file[[4]] <- file[[4]] * 0.0001
#  file[[5]] <- file[[5]] * 0.0001
#  ## Only keep clear observations (QA band 10 codes 66 or 130)
#  clear_code <- c(66, 130)
#  bad_pixels <- which(!(values(file[[10]]) %in% clear_code))
#  values(file[[1]])[bad_pixels] <- NA
#  values(file[[3]])[bad_pixels] <- NA
#  values(file[[4]])[bad_pixels] <- NA
#  values(file[[5]])[bad_pixels] <- NA
#  ## calculate EVI
#  file[[12]] <- 2.5 * ((file[[4]] - file[[3]]) / (file[[4]] + 6 * file[[3]] - 7.5 * file[[1]] + 1))
#  ## calculate LSWI
#  file[[13]] <- (file[[4]] - file[[5]]) / (file[[4]] + file[[5]])
#  ## convert raster data to data.table and assign Day of Year values
#  ## EVI
#  EVI.dt = as.data.table(as.data.frame(file[[12]], xy=T))
#  EVI.dt = cbind(1:ncell(file[[12]]), EVI.dt)
#  setnames(EVI.dt,c("Index","x","y", "EVI"))
#  setkey(EVI.dt,Index,x,y)
#  ## LSWI
#  LSWI.dt = as.data.table(as.data.frame(file[[13]], xy=T))
#  LSWI.dt = cbind(1:ncell(file[[13]]), LSWI.dt)
#  #LSWI.dt$DOY = yday(ymd(paste0('2018',substr((names(file))[1],3,6))))
#  # Line above did not work, see comment in previous loop
#  #TPD:
#  #LSWI.dt$DOY = yday(ymd(paste0('2018',substr(ls7[i],12,15))))
#  #Borden:
#  LSWI.dt$DOY = yday(ymd(paste0('2018',substr(ls7[i],3,6))))
#  setnames(LSWI.dt,c("Index","x","y", "LSWI","DOY"))
#  setkey(LSWI.dt,Index,x,y)
#  ## save a data.table for each image
#  assign(paste0('EVI_LSWI',i,'_7.dt'), merge(EVI.dt,LSWI.dt,by=c("Index","x","y")))
#}



## merge daily files into one for all of 2018 
## MAKE SURE TO CHANGE FOR EACH SITE (some locations have more/fewer overpasses)
#EVI_LSWI.dt <- rbind(EVI_LSWI1_7.dt,EVI_LSWI2_7.dt,EVI_LSWI3_7.dt,EVI_LSWI4_7.dt,EVI_LSWI5_7.dt,EVI_LSWI6_7.dt,
#                     EVI_LSWI7_7.dt,EVI_LSWI8_7.dt,EVI_LSWI9_7.dt,EVI_LSWI10_7.dt,EVI_LSWI11_7.dt, EVI_LSWI12_7.dt,
#                     EVI_LSWI13_7.dt,EVI_LSWI14_7.dt,
#                     EVI_LSWI1_8.dt,EVI_LSWI2_8.dt,EVI_LSWI3_8.dt,EVI_LSWI4_8.dt,EVI_LSWI5_8.dt,EVI_LSWI6_8.dt,
#                     EVI_LSWI7_8.dt,EVI_LSWI8_8.dt,EVI_LSWI9_8.dt,EVI_LSWI10_8.dt,EVI_LSWI11_8.dt,EVI_LSWI12_8.dt,
#                     EVI_LSWI13_8.dt,EVI_LSWI14_8.dt)

## order by DOY
#EVI_LSWI.dt <- EVI_LSWI.dt[order(EVI_LSWI.dt$DOY),]

## convert to dataframe
#EVI_LSWI.df <- as.data.frame(EVI_LSWI.dt)

## There are a few erroneous values with EVI = 2.5. These are omitted from the dataset 
#EVI_LSWI.df[which(EVI_LSWI.df$EVI > 1),'EVI'] <- NA

## Set up new dataframe for EVI/LSWI Interpolation
#npixel <- length(unique(EVI_LSWI.df$Index))
#inter_evi_lswi <- as.data.frame(matrix(ncol = 2, nrow = npixel*365))
#colnames(inter_evi_lswi) <- c('Index', 'DOY')
#inter_evi_lswi[,1] <- rep(seq(1,npixel,1), 365)
#inter_evi_lswi <- inter_evi_lswi[order(inter_evi_lswi[,1]),]
#inter_evi_lswi[,2] <- rep(seq(1,365,1), npixel)
#inter_evi_lswi <- merge(inter_evi_lswi, EVI_LSWI.df[,c('Index', 'EVI', 'LSWI', 'DOY')], by = c('Index', 'DOY'), all = TRUE)
#inter_evi_lswi  <- inter_evi_lswi[order(inter_evi_lswi[,'Index'],inter_evi_lswi [,'DOY']),]

# add coordinates
#x <- as.data.frame(tapply(EVI_LSWI.df$x, EVI_LSWI.df$Index, mean, na.rm = T))
#y <- as.data.frame(tapply(EVI_LSWI.df$y, EVI_LSWI.df$Index, mean, na.rm = T))
#xy <- cbind(as.numeric(rownames(x)), x, y)
#colnames(xy) <- c('Index', 'x', 'y')

#inter_evi_lswi <- merge(inter_evi_lswi, xy, by = 'Index', all = TRUE)
#inter_evi_lswi <- inter_evi_lswi[,c('Index', 'DOY', 'x', 'y', 'EVI', 'LSWI')]

# Interpolate daily EVI values for each pixel
#inter_evi_lswi$EVI_inter <- NA

#for(i in unique(inter_evi_lswi$Index)){
#  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$EVI)),'EVI']) > 3){
#    pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
#    spl <- with(pix[!is.na(pix$EVI),],smooth.spline(DOY,EVI, spar = .25)) 
#    inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- predict(spl, c(1:365))$y
#  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- NA}
#  print(round(i/npixel*100, 1))
#}

# Interpolate daily LSWI values for each pixel
#inter_evi_lswi$LSWI_inter <- NA

#for(i in unique(inter_evi_lswi$Index)){
#  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$LSWI)),'LSWI']) > 3){
#    pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
#    spl <- with(pix[!is.na(pix$LSWI),],smooth.spline(DOY,LSWI, spar = .25)) 
#    inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- predict(spl, c(1:365))$y
#  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- NA}
#  print(round(i/npixel*100, 1))
#}

#write.table(inter_evi_lswi,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden_500m/adjusted_evi_lswi_interpolated_modis.csv',row.names = F,
#            sep=',')




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
#inter_evi_lswi$LSWI_inter <- NA

#for(i in unique(inter_evi_lswi$Index)){
#  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$LSWI)),'LSWI']) > 3){
#    pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
#    spl <- with(pix[!is.na(pix$LSWI),],smooth.spline(DOY,LSWI, spar = .25)) 
#    inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- predict(spl, c(1:365))$y
#  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- NA}
#  print(round(i/npixel*100, 1))
#}

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
#write.table(inter_evi_lswi,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden/adjusted_evi_lswi_interpolated_ls7and8.csv',row.names = F,
#            sep=',')
