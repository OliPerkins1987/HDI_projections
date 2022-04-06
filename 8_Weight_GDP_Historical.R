

library(raster)
library(ncdf4)
library(rgdal)

############################################################################################

### Read data

############################################################################################

setwd('F:/PhD/Data/Future_projections/HDI/completed')

Education    <- lapply(1:5, function(i) {
                  filt <- grepl(paste0('SSP', i), list.files(pattern = 'Education_'))
                  brick(list.files(pattern = 'Education_')[filt])
                    })

Health       <- lapply(1:5, function(i) {
                  filt <- grepl(paste0('SSP', i), list.files(pattern = 'Health_'))
                    brick(list.files(pattern = 'Health_')[filt])
                    })

HDI.pred <- lapply(1:5, function(i) {
              sqrt(Education[[i]] * Health[[i]])
              })

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Processed secondary/Merged with JULES')
JULES.mask         <- raster('JULES_Mask.tif')
extent(JULES.mask) <- c(-180, 180, -90, 90)
base_dir           <- getwd()

setwd(paste0(base_dir, '/Socio economic/GDP and HDI'))
HDI            <- brick('HDI_JULES.tif')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Completed_rasters')
GDP <- brick('GDP_pc_SSP2.nc')

#############################################################################################

### Compare

#############################################################################################

### All years

compare <- cbind(as.data.frame(HDI, long = T), 
                 as.data.frame(HDI.pred[[2]][[1:26]], long = T), 
                 as.data.frame(GDP[[1:26]], long = T)) 

compare           <- compare[, c(2, 4, 6)]
colnames(compare) <- c('Historical', 'Predicted', 'GDP')
#compare$GDP       <- (compare$GDP - 100) / (75000-100)
#compare$GDP       <- ifelse(compare$GDP < 0, 0, 
#                      ifelse(compare$GDP > 1, 1, compare$GDP))

compare$Year <- rep(1990:2015, each = 27648)
all.compare  <- compare
compare      <- compare[complete.cases(compare), ]

1 - (sqrt(mean((compare$Predicted - compare$Historical)^2, na.rm = T)) / 
       sd(compare$Historical -mean(compare$Historical, na.rm = T), na.rm = T))

summary(lm(compare$Historical ~ compare$Predicted + log1p(compare$GDP)))
fit <- lm(Historical ~ Predicted + log1p(GDP), data = compare)
plot(fit$fitted.values, fit$fitted.values + fit$residuals)

test         <- JULES.mask
values(test) <- predict(fit, all.compare[all.compare$Year == 2015, ])
plot((test - HDI[[26]]) * (abs(test - HDI[[26]]) > 0.1))


###############################################################################################

### Account for economic index

###############################################################################################

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Completed_rasters')
GDP  <- lapply(1:5, function(i) {
          filt <- grepl(paste0('SSP', i), list.files(pattern = 'GDP_'))
            brick(list.files(pattern = 'GDP_')[filt])
              })

for(i in c(1:5)) {

compare <- cbind(as.data.frame(HDI.pred[[i]], long = T), 
                 as.data.frame(GDP[[i]], long = T)) 

compare <- compare[, c(2, 4)]
colnames(compare) <- c('Predicted', 'GDP')

prediction.rast <- predict(fit, compare)

prediction.rast <- split(prediction.rast, rep(1:111, each = 27648))
prediction.rast <- brick(lapply(prediction.rast, function(x) {setValues(JULES.mask, x)}))

plot(sapply(1:111, function(x) {mean(prediction.rast[[x]][], na.rm = T)}))
plot(prediction.rast[[61]])

setwd('F:/PhD/Data/Future_projections/HDI/completed')
writeRaster(prediction.rast, paste0('Economic_adjusted_SSP',i, '.nc'))



}
