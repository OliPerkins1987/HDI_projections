
library(raster)
library(rgdal)
library(tidyverse)
library(countrycode)


setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Cleaned Future')

Health    <- read.csv('Future_Health_SSP2.csv')
Education <- read.csv('Future_Education_to2100.csv')

###?
#Econ      <- read.csv('Cleaned_GDP_Remmitance.csv')


setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI')
HDI.shape           <- readOGR('GDL Shapefiles V4.shp')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Processed secondary/Merged with JULES')
JULES.mask         <- raster('JULES_Mask.tif')
extent(JULES.mask) <- c(-180, 180, -90, 90)
base_dir           <- getwd()

setwd(paste0(base_dir, '/Socio economic/GDP and HDI'))
HDI            <- brick('HDI_JULES.tif')
GDP            <- brick('GDP_JULES.tif')

#setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/Population')
#Econ.grid <- readOGR('grid.shp')


setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Cleaned Historical')

Health_past    <- read.csv('Cleaned_Regional_Health.csv')
Education_past <- read.csv('Cleaned_Regional_Education.csv') 
GNI_past       <- read.csv('Cleaned_Regional_GNI.csv')
HDI_past       <- read.csv('Cleaned_Regional_HDI.csv')
HDI_past$HDI_nat_reg_ratio <- HDI_past$HDI / HDI_past$National_HDI

####################################################################################

### Assumptions about Econ index

####################################################################################

Dim.index <- function(x, maximum, minimum) {
  
  y <- (x - minimum) / (maximum - minimum)
  
  y <- ifelse(y > 1, 1, y)
  y <- ifelse(y < 0, 0, y)
  
  y
  
}


Health$value        <- Dim.index(Health$value, 85, 20)
colnames(Health)[3] <- 'Health_index'


####################################################################################

### Merge data into one shape

####################################################################################

Education_Future      <- Education[Education$Year %in% c(2010, 2015, 2020), ]
Education_Future$Year <- ifelse(Education_Future$Year == 2020, 2019, Education_Future$Year)  
Health_Future         <- Health[Health$Year == 2020, ]
Health_Future$Year    <- 2019

Education_past        <- Education_past[Education_past$Year %in% c(2010, 2015, 2019), ] 
Health_past           <- Health_past[Health_past$Year == 2019, ]


Edu_compare    <- merge(Education_past, Education_Future[, c(1, 2, 7)], 
                     by.x = c('ISO_Code', 'Year'), by.y = c('REGION', 'Year'))

Health_compare <- merge(Health_past, Health_Future, 
                        by.x = 'ISO_Code', by.y = 'ISO')

HDI_compare    <- merge(HDI_past, Education_Future[, c(1, 2, 7)], 
                        by.x = c('ISO_Code', 'Year'), by.y = c('REGION', 'Year'), all.x = T)

HDI_compare    <- merge(HDI_compare, Health_Future[, c(1, 2, 3)], 
                        by.x = c('ISO_Code', 'Year'), by.y = c('ISO', 'Year'), all.x = T)

colnames(HDI_compare)[6]     <- 'Health_index'
colnames(HDI_compare)[18:19] <- c('Education_projection', 'Health_projection')
HDI_compare                  <- HDI_compare[HDI_compare$Year == 2019, ]

####################################################################

### Compare National data for overlapping years

####################################################################

### national conversion factors from linear models 

### Health
summary(lm(Health_compare$National_index ~ Health_compare$Health_index.y))
plot((-0.036778 + Health_compare$Health_index.y * 1.031733), Health_compare$National_index)
health.lm <- lm(Health_compare$National_index ~ Health_compare$Health_index.y)

### Education
summary(lm(Edu_compare$National_index ~ Edu_compare$Education_Index))
edu.lm(lm(Edu_compare$National_index ~ Edu_compare$Education_Index))
plot((0.154792 + 0.911959 * Edu_compare$Education_Index), Edu_compare$National_index)
abline(0, 1)

### try continent variation
Edu_compare$WB_region <- countrycode(Edu_compare$ISO_Code, 'iso3c', 'region')


### hierachical
summary(lm(Edu_compare$National_index ~ Edu_compare$Education_Index + Edu_compare$WB_region))
cont.lm <- lm(National_index ~ Education_Index + WB_region, data = Edu_compare)
plot(cont.lm$fitted.values, cont.lm$fitted.values + cont.lm$residuals)

### apply model
Edu_compare$Adjusted_Edu_projection <- cont.lm$fitted.values

#################
### Combine data
#################

### apply correction from individual factors

HDI_compare <- merge(HDI_compare, Edu_compare[, c(2, 5, 11:12)])
HDI_compare <- HDI_compare[, c(1:5, 20, 6:19, 21)]

### apply health correction
HDI_compare$Adjusted_Health_projection <- (-0.036778 + 1.031733 * HDI_compare$Health_projection)
