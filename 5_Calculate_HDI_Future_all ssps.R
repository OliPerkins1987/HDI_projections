
library(raster)
library(rgdal)
library(tidyverse)
library(zoo)
library(countrycode)


setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Cleaned Future')

Health         <- read.csv('Future_Health_SSP2.csv')
Education      <- read.csv('Future_Education_All_SSPs.csv')
colnames(Education)[c(1, 8)] <- c('ISO', 'Education_Index')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Projection tools')
Prediction_key           <-  read.csv('HDI_harmonisation_key.csv')
Prediction_key           <- Prediction_key[, colnames(Prediction_key) != 'X']

### merge in worldbank region
Education$WB_region <- countrycode(Education$ISO, 'iso3c', 'region')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Projection tools')
Edu_SSP_key    <- read.csv('Education_SSP_key.csv')
Health_SSP_key <- read.csv('Health_SSP_key.csv')
Gini           <- read.csv('GINI_adjustment.csv')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Cleaned Historical')

Health_past    <- read.csv('Cleaned_Regional_Health.csv')
Education_past <- read.csv('Cleaned_Regional_Education.csv') 


##########################################################################################

### 1) Future Health

##########################################################################################

calc_index <- function(x, maximum, minimum, limits = T) {
  
  y <- (x - minimum) / (maximum - minimum)
  
  if(limits == T) {
  
  y <- ifelse(y > 1, 1, y)
  y <- ifelse(y < 0, 0, y)
  
  }
  
  y
  
}


### calculates health index for the future

Calculate_health_index <- function(dat, thresholds = c(85, 20), lin_adjust = T) {
  
  dat$index <- calc_index(dat$value, thresholds[1], thresholds[2], limits = T)
  
  ### linear adjustment of error
  
  if(lin_adjust == T){
  
  dat$index <- (-0.036778 + 1.031733 * dat$index)
  
  }

  return(dat)
  
}


### calculates index @ subnational level

subnat_health_index <- function(dat, key, fill_missing = T, error_adjust = F) {
  
  ### combine data
  x <- merge(key[, colnames(key) %in% c('ISO_Code', 'GDLCODE', 'Health_nat_reg_ratio', 'local_error')], dat, 
             by.x = 'ISO_Code', by.y = 'ISO', all.x = T)
  
  
  missing.list <- list('XKO' = c('SRB', 'ALB', 'MKD', 'MNE'), 
                       'SYR' = c('IRQ', 'LBN')) 

  
  ### fill missing values - here just Kosovo - from surrounding countries
  if(fill_missing == T) {
    
    
  for(i in 1:length(missing.list)) {
    
    g <- expand.grid(names(missing.list)[i],x$GDLCODE[x$ISO_Code == names(missing.list)[i]], 
                     as.numeric(unique(x$Year)))
    
    g <- g[complete.cases(g), ]
    
    colnames(g) <- c('ISO_Code','GDLCODE', 'Year')
    
    reps <- nrow(g) / length(x$Health_nat_reg_ratio[x$ISO_Code == names(missing.list)[i]])
    
    g$Health_nat_reg_ratio <- rep(x$Health_nat_reg_ratio[x$ISO_Code == names(missing.list)[i]], times = reps)
    g$local_error          <- rep(x$local_error[x$ISO_Code == names(missing.list)[i]], times = reps)
    
    g$value <- NA
    g$index <- NA
    
    for(r in 1:nrow(g)){
      
      g$value[r] <- mean(x$value[x$ISO_Code %in% missing.list[[i]] & x$Year == g$Year[r]])
      g$index[r] <- mean(x$index[x$ISO_Code %in% missing.list[[i]] & x$Year == g$Year[r]])
    }
    
    x <- x[x$ISO_Code != names(missing.list)[i], ]
    x <- rbind(x, g)
  
    
    }
    
    x <- x[complete.cases(x), ]
    
  }
  
  ######################
  ### constrain prediction
  ######################
  
  x$Health_prediction   <- x$index
  
  
  if(error_adjust == T) {
    
    x$Health_prediction <- x$Health_prediction * x$local_error
    
  }
  
  ### max val of 1!
  
  x$Health_prediction   <- ifelse(x$Health_prediction > 1, 1, x$Health_prediction)
  x$Health_prediction   <- ifelse(x$Health_prediction < 0, 0, x$Health_prediction)
  
  return(x)
  
}


x <- Calculate_health_index(Health)
x <- subnat_health_index(x, Prediction_key, fill_missing = F)

########################################################################################

### Downscaling

########################################################################################

### GINI based
x$WB                         <- countrycode(x$ISO_Code, 'iso3c', 'region')
test                         <- merge(x[, c(1:3, 5:(ncol(x)))], Health_SSP_key[, c(1, 3, 6:10)], 
                                    by = c('GDLCODE', 'Year'))
test                         <- merge(test, Gini[, -4], by = c('SCENARIO', 'Year', 'WB')) 
test$Nat_reg_ratio <- ifelse(is.na(test$delta), 
                             test$Health_nat_reg_ratio, (1 + (test$Health_nat_reg_ratio - 1) * test$delta)) 


test %>% filter(test$ISO_Code != 'SYR') %>%
  mutate(SCENARIO = factor(SCENARIO)) %>%
  group_by(.dots = c('Year', 'SCENARIO')) %>%
  summarise(index = mean(Health_prediction)) %>%
    ggplot(aes(x = Year, y = index, colour = SCENARIO)) + geom_line()


x <- test



##############################################################################################

### Interpolation - linear

##############################################################################################

new.dat           <- expand.grid(unique(x$GDLCODE),  unique(x$SCENARIO), 2020:2095)
colnames(new.dat) <- c('GDLCODE', 'SCENARIO','Year')
new.dat           <- merge(new.dat, x, by = c('GDLCODE','SCENARIO','Year'), all.x= T)

new.dat             <- new.dat[order(new.dat$GDLCODE, new.dat$SCENARIO, new.dat$Year), ]
new.dat$ISO_Code    <- na.locf(new.dat$ISO_Code)
new.dat$WB          <- na.locf(new.dat$WB)
new.dat$Tier        <- na.locf(new.dat$Tier)
new.dat$Country_tier<- na.locf(new.dat$Country_tier)
new.dat             <- new.dat[, -c(10)]
new.dat[, c(6:10, 13, 14)] <- apply(new.dat[, c(6:10, 13, 14)], 2, na.approx)         


new.dat$Health_prediction <- new.dat$index * new.dat$Nat_reg_ratio * new.dat$SSP.Adjustment
new.dat$Health_prediction <- ifelse(new.dat$Health_prediction > 1, 1, 
                                 ifelse(new.dat$Health_prediction <0, 0, 
                                        new.dat$Health_prediction))

new.dat %>% group_by(.dots = c('Year', 'SCENARIO')) %>% 
  summarise(index = mean(Health_prediction)) %>% 
  ggplot(aes(x = Year, y = index, colour = SCENARIO)) + 
  geom_point(alpha = 0.1) + theme(text = element_text(size=16)) + 
  geom_smooth(size = 3) + theme_classic() + scale_colour_viridis_d()

#setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Projections')
#write.csv(new.dat, 'Health_predictions_allSSPs_Gini.csv', row.names = F)


###################################################################################################

### Future Education

###################################################################################################


Calculate_edu_index <- function(dat, 
                                thresholds = list('mean' = c(0, 15), 'future' = mean(c(0, 18))), 
                                lin_adjust = T) {
  
  ### linear adjustment of error
  
  if(lin_adjust == T){
    
    dat$index <- (0.20850 + 0.76773 * dat$Education_Index)
    
    dat$index <- ifelse(dat$WB_region == 'Europe & Central Asia', 
                  dat$index + 0.06293 * dat$index, 
                    ifelse(dat$WB_region == 'Latin America & Caribbean',
                     dat$index + 0.03829 * dat$index,  
                       ifelse(dat$WB_region == 'Middle East & North Africa',
                        dat$index + 0.01459 * dat$index,
                          ifelse(dat$WB_region == 'North America',
                            dat$index + 0.08542 * dat$index,
                             ifelse(dat$WB_region == 'South Asia',
                               dat$index + 0.01963 * dat$index,
                                ifelse(dat$WB_region == 'Sub-Saharan Africa',
                                 dat$index - 0.02289 * dat$index,
                                  dat$index))))))
    
    

  }
  
  dat
  
}

x <- Calculate_edu_index(Education)


#######################################

### fill missing

#######################################


fill_edu_index <- function(dat, key, fill_missing = T) {
  
  ### combine data
  x <- merge(key[, colnames(key) %in% c('ISO_Code', 'GDLCODE', 'Education_nat_reg_ratio', 'local_error')], 
             dat[, !colnames(dat) == 'WB_region'], 
             by.x = 'ISO_Code', by.y = 'ISO', all.x = T)
  
  missing.list <- list('XKO' = c('SRB', 'ALB', 'MKD', 'MNE'), 
                       'SSD' = c('SDN', 'CAF', 'ETH', 'UGA'), 
                       'KIR' = c('FJI'))
  
  
  ### fill missing values - here just Kosovo - from surrounding countries
  if(fill_missing == T) {
    
    for(i in 1:length(missing.list)) {
    
      print(names(missing.list)[i])
      
    g <- expand.grid(names(missing.list)[i],x$GDLCODE[x$ISO_Code == names(missing.list)[i]], 
                     as.numeric(unique(x$Year)), unique(x$SCENARIO))
    
    g <- g[complete.cases(g), ]
    
    colnames(g) <- c('ISO_Code','GDLCODE', 'Year', 'SCENARIO')
    
    reps <- nrow(g) / length(x$Education_nat_reg_ratio[x$ISO_Code == names(missing.list)[i]])
    
    g$Education_nat_reg_ratio <- rep(x$Education_nat_reg_ratio[x$ISO_Code == names(missing.list)[i]], 
                                     times = reps)
    g$local_error             <- rep(x$local_error[x$ISO_Code == names(missing.list)[i]], 
                                     times = reps)
    
    g$Expected_schooling <- NA
    g$Mean_schooling     <- NA
    g$FS_index           <- NA
    g$MS_index           <- NA
    g$Education_Index    <- NA
    g$index              <- NA
    
    
    for(s in unique(g$SCENARIO)) {
      
      filt                  <- x$SCENARIO == s
    
      for(r in 1:nrow(g)){
      
        
      g$Expected_schooling[r] <- mean(x$Expected_schooling[filt & x$ISO_Code %in% missing.list[[i]] & x$Year == g$Year[r]])
      g$Mean_schooling[r]     <- mean(x$Mean_schooling[filt & x$ISO_Code %in% missing.list[[i]] & x$Year == g$Year[r]])
      g$FS_index[r]           <- mean(x$FS_index[filt & x$ISO_Code %in% missing.list[[i]] & x$Year == g$Year[r]])
      g$MS_index[r]           <- mean(x$MS_index[filt & x$ISO_Code %in% missing.list[[i]] & x$Year == g$Year[r]])
      g$Education_Index[r]    <- mean(x$Education_Index[filt & x$ISO_Code %in% missing.list[[i]] & x$Year == g$Year[r]])
      g$index[r]              <- mean(x$index[filt & x$ISO_Code %in% missing.list[[i]] & x$Year == g$Year[r]])
      
      
      
      }
      
      print(s)
      
    }
    
    ### match cols
    
    
    x <- x[x$ISO_Code != names(missing.list)[i], ]
    x <- rbind(x, g)
    
    
    }
  
    
  }
  
  x$Education_nat_reg_ratio[is.na(x$Education_nat_reg_ratio)] <- 1
  
  return(x)
  
}

x <- fill_edu_index(x, Prediction_key, fill_missing = F)
x <- x[!duplicated(x), ]

### check
x %>% filter(Year == 2050) %>%
  split(x$SCENARIO[x$Year == 2050]) %>%
    lapply(function(y) {summary(y$index)})

##############################################################

### Create scenario weights

##############################################################

################################################################################################

### GINI-based weights

################################################################################################
x$WB                         <- countrycode(x$ISO_Code, 'iso3c', 'region')
x$SCENARIO                   <- substr(x$SCENARIO, 1, 4)
test                         <- merge(x, Gini[, -4], by = c('SCENARIO', 'Year', 'WB')) 
test$Nat_reg_ratio <- ifelse(is.na(test$delta), 
                                  test$Education_nat_reg_ratio, (1 + (test$Education_nat_reg_ratio - 1) * test$delta)) 

x <- test

#######################################

### make prediction

#######################################

subnat_edu_index <- function(dat, error_adjust = F) {

dat$Education_prediction <- dat$index * dat$Nat_reg_ratio


if(error_adjust == T) {
  
  dat$Education_prediction <- dat$Education_prediction * dat$local_error
  
}


dat$Education_prediction   <- ifelse(dat$Education_prediction > 1, 1, dat$Education_prediction)
dat$Education_prediction   <- ifelse(dat$Education_prediction < 0, 0, dat$Education_prediction)

return(dat)


}

x <- subnat_edu_index(x)

##############################################################################################

### Interpolation - linear

##############################################################################################

new.edu           <- expand.grid(unique(x$GDLCODE), 2020:2100, unique(x$SCENARIO))
colnames(new.edu) <- c('GDLCODE', 'Year', 'SCENARIO')
new.edu           <- merge(new.edu, x, by = c('GDLCODE', 'Year', 'SCENARIO'), all.x= T)
new.edu           <- new.edu[order(new.edu$GDLCODE, new.edu$SCENARIO, new.edu$Year), ]

new.edu$ISO_Code    <- na.locf(new.edu$ISO_Code)
new.edu$WB          <- na.locf(new.edu$WB)
new.edu[, c(6:15)]  <- apply(new.edu[, c(6:15)], 2, na.approx)                      


new.edu %>% group_by(.dots = c('Year', 'SCENARIO')) %>% 
  summarise(Education = mean(index)) %>% 
   ggplot(aes(x = Year, y = Education, colour = SCENARIO)) + 
  geom_point(alpha = 0.1) + theme(text = element_text(size=16)) + 
  geom_smooth(size = 3) + theme_classic() + scale_colour_viridis_d()

#setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Projections')
#write.csv(new.edu, 'Education_predictions_allSSPs_GINIcoef.csv', row.names = F)

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI')
HDI.shape        <- readOGR('GDL Shapefiles V4.shp')
new.edu          <- pivot_wider(new.edu[, c(1, 2, 3, 16)], 
                                names_from = SCENARIO, values_from = Education_prediction)
HDI.shape@data   <- merge(HDI.shape@data, new.edu[new.edu$Year == 2100, ], 
                          by.x = 'GDLcode', by.y = 'GDLCODE', all.x = T)

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Processed secondary/Merged with JULES')
JULES.mask         <- raster('JULES_Mask.tif')
extent(JULES.mask) <- c(-180, 180, -90, 90)

edu.rast <- lapply(colnames(HDI.shape@data)[8:12], 
       function(x) {rasterize(HDI.shape, JULES.mask, 
                             field = x, fun = 'mean', na.rm = T)})

names(edu.rast) <- substr(colnames(HDI.shape@data)[8:12], 1, 4)
edu.rast <- brick(edu.rast)
plot(edu.rast, zlim = c(0, 1))
