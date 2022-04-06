

library(raster)
library(rgdal)

############################################################################################

### Read data

############################################################################################

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Completed_data')

Edu    <- read.csv('Education_harmonized_2010baseline.csv')
Health <- read.csv('Health_harmonized_2010baseline.csv')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI')
HDI.shape        <- readOGR('GDL Shapefiles V4.shp')
res.shape        <- HDI.shape

### Read in your mask here!!

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Processed secondary/Merged with JULES')
JULES.mask         <- raster('JULES_Mask.tif')
extent(JULES.mask) <- c(-180, 180, -90, 90)
base_dir           <- getwd()



#############################################################################################

### Make maps

#############################################################################################

setwd('F:/PhD/Data/Future_projections/HDI')

for(x in c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5')) {
  
  for(y in 1990:2095) {
  
  res.shape@data <- merge(HDI.shape@data, Health[Health$SCENARIO == x & Health$Year == y, ], 
                     by.x = 'GDLcode', by.y = 'GDLCODE', all.x = T)

  res.shape@data$Subnat_index[res.shape@data$country == 'Sri Lanka']             <- res.shape@data[res.shape@data$region == 'Tamil Nadu' & !is.na(res.shape@data$region), 12]
  res.shape@data$Subnat_index[res.shape@data$country == 'Oman']                  <- res.shape@data[res.shape@data$region == 'Center (Riadh, Qassim)' & !is.na(res.shape@data$region), 12]
  res.shape@data$Subnat_index[res.shape@data$country == 'United Arab Emirates']  <- res.shape@data[res.shape@data$region == 'Center (Riadh, Qassim)' & !is.na(res.shape@data$region), 12]
  res.shape@data$Subnat_index[res.shape@data$country == 'Cyprus']                <- mean(res.shape@data[res.shape@data$country == 'Greece' & !is.na(res.shape@data$region), 12], na.rm = T)
  res.shape@data$Subnat_index[res.shape@data$country == 'Iceland']               <- mean(res.shape@data[res.shape@data$country == 'Norway' & !is.na(res.shape@data$region), 12], na.rm = T)
  
  temp <- rasterize(res.shape, JULES.mask, field = 'Subnat_index', fun = 'mean', 
                       na.rm = T)

  r2 <- focal(temp, w = matrix(1,9,9), fun = mean, 
              pad = F, na.rm = T)
  
  temp[is.na(temp) & JULES.mask >0] <- r2[is.na(temp) & JULES.mask>0]
  
  
  writeRaster(temp, paste0('Health_', x, '_',y,'.nc'), overwrite  =T)

  plot(temp)
  
  remove(temp)
  gc()
  
  print(y)
  
  }

  print(x)
  
}


######################################################################################################

### Compile

######################################################################################################

setwd('F:/PhD/Data/Future_projections/HDI')

Health    <- list.files(pattern = 'Health_')
Health    <- split(Health, substr(Health, 8, 11))

Education <- list.files(pattern = 'Education_')
Education <- split(Education, substr(Education, 11, 14))


Health    <- lapply(Health, function(x) {
               brick(lapply(x, function(z) {
                raster(z)}))
                  })

Education <- lapply(Education, function(x) {
               brick(lapply(x, function(z) {
                raster(z)}))
                  })

Health <- lapply(Health, function(x) {
  
  x <- addLayer(x, x[[106]])
  x <- addLayer(x, x[[106]])
  x <- addLayer(x, x[[106]])
  x <- addLayer(x, x[[106]])
  x <- addLayer(x, x[[106]])
  
  brick(x)
  
})

Education <- lapply(Education, function(x) {
  
  x <- addLayer(x, x[[106]])
  x <- addLayer(x, x[[106]])
  x <- addLayer(x, x[[106]])
  x <- addLayer(x, x[[106]])
  x <- addLayer(x, x[[106]])
  
  brick(x)
  
})


### write out

setwd('F:/PhD/Data/Future_projections/HDI/completed')

for(i in 1:length(Health)) {
  
  writeRaster(Health[[i]], paste0('Health_SSP', i, '.nc'), overwrite = T)
  #writeRaster(Education[[i]], paste0('Education_SSP', i, '.nc'))
  
}




