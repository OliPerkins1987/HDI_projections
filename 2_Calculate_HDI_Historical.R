
library(raster)
library(rgdal)
library(tidyverse)


setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Cleaned Historical')

Health    <- read.csv('Cleaned_Regional_Health.csv')
Education <- read.csv('Cleaned_Regional_Education.csv')
Econ      <- read.csv('Cleaned_GDP_Remmitance.csv')
GNI       <- read.csv('Cleaned_Regional_GNI.csv')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI')
HDI.shape           <- readOGR('GDL Shapefiles V4.shp')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Processed secondary/Merged with JULES')
JULES.mask         <- raster('JULES_Mask.tif')
extent(JULES.mask) <- c(-180, 180, -90, 90)
base_dir           <- getwd()

setwd(paste0(base_dir, '/Socio economic/GDP and HDI'))
HDI            <- brick('HDI_JULES.tif')
GDP            <- brick('GDP_JULES.tif')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/Population')
Econ.grid <- readOGR('grid.shp')

setwd('')


####################################################################################

### Assumptions about Econ index

####################################################################################

Dim.index <- function(x, maximum, minimum) {
  
  y <- (x - minimum) / (maximum - minimum)
  
  y <- ifelse(y > 1, 1, y)
  y <- ifelse(y < 0, 0, y)
  
  y
  
}


Econ$Econ_index <- Dim.index(log(Econ$GDP_remmitance_pc), 
                             log(40000), log(100))

Econ <- Econ %>% arrange(py) %>% arrange(py)   

### assume NA is 0?
Econ.grid@data <- merge(Econ.grid@data, Econ[Econ$name == 2010, c(1, 9)], by = 'gID')
Econ.pols      <- over(HDI.shape, Econ.grid, fn = 'mean', na.rm = T)

####################################################################################

### Merge data into one shape

####################################################################################

Education_2010      <- Education[Education$name == 2010, ]
Health_2010         <- Health[Health$name == 2010, ]
GNI_2010            <- GNI[GNI$name == 2010, ]

HDI.shape@data      <- merge(HDI.shape@data, Education_2010[, c(5, 7)], all.x = T, 
                             by.x = 'GDLcode', by.y = 'GDLCODE')

HDI.shape@data      <- merge(HDI.shape@data, Health_2010[, c(5, 7)], all.x = T, 
                             by.x = 'GDLcode', by.y = 'GDLCODE')

HDI.shape@data      <- merge(HDI.shape@data, GNI_2010[, c(5, 7)], all.x = T, 
                             by.x = 'GDLcode', by.y = 'GDLCODE')

HDI.shape@data$Econ_index <- Econ.pols$Econ_index

#########################################
### Fill missing values from regional
#########################################


#######################################################################

### Rasterize

#######################################################################

Health.rast    <- rasterize(HDI.shape, JULES.mask, field = 'Health_index', fun = 'mean')
Education.rast <- rasterize(HDI.shape, JULES.mask, field = 'Education_index', fun = 'mean')
Econ.rast      <- rasterize(HDI.shape, JULES.mask, field = 'Econ_index', fun = 'mean')
GNI.rast       <- rasterize(HDI.shape, JULES.mask, field = 'GNI_index', fun = 'mean')

####################################################################################

### Calc HDI

####################################################################################

HDI.rast <- (GNI.rast * Health.rast * Education.rast)^(1/3)

### compare with Kummu et al 2017
HDI.frame <- data.frame(mod = HDI.rast[], Kummu = HDI[[21]][])
HDI.frame <- HDI.frame[complete.cases(HDI.frame), ]

Metrics::mae(HDI.frame$mod, HDI.frame$Kummu)

### 2 variable HDI performs better than remittance adjusted GDP as economic
