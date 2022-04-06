
library(raster)
library(rgdal)
library(tidyverse)
library(zoo)
library(countrycode)
library(smooth)

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Cleaned Future')

Health         <- read.csv('Future_Health_SSP2.csv')
Education      <- read.csv('Future_Education_All_SSPs.csv')
colnames(Education)[c(1, 8)] <- c('ISO', 'Education_Index')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Health')
Health_SSP_key <- read.csv('Health_All_SSPs.csv')


setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Projection tools')
Prediction_key           <-  read.csv('HDI_harmonisation_key.csv')
Prediction_key           <- Prediction_key[, colnames(Prediction_key) != 'X']

### merge in worldbank region
Education$WB_region <- countrycode(Education$ISO, 'iso3c', 'region')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Cleaned Historical')

Health_past    <- read.csv('Cleaned_Regional_Health.csv')
Education_past <- read.csv('Cleaned_Regional_Education.csv') 

### GINI Coef
setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Gini')

GINI <- read.csv('Gini_projections_SSPs.csv')


##########################################################################################

### Gini by country by year

##########################################################################################

GINI <- pivot_longer(GINI, colnames(GINI)[3:ncol(GINI)])
colnames(GINI) <- c('SCENARIO', 'Year', 'ISO_Code', 'GINI')

GINI.summary <- GINI %>% 
  mutate(WB = countrycode(ISO_Code, 'iso3c', 'region')) %>%
  group_by(.dots = c('SCENARIO', 'Year', 'WB')) %>%
  summarise(index = mean(GINI)) %>% ungroup()

ggplot(GINI.summary, aes(x = Year, y = index, colour = WB)) +
  scale_x_continuous(breaks = c(2010, 2050, 2100)) + 
   geom_line() + facet_grid(SCENARIO~.)

GINI.summary <- GINI.summary %>%
  split(GINI.summary$WB) %>%
   lapply(function(x) {
     
     x %>% 
       split(x$SCENARIO) %>%
       lapply(function(y){
        mutate(y, delta = dplyr::first(index))
       }) %>% plyr::rbind.fill()
           
   }) %>% plyr::rbind.fill()

GINI.summary$delta <- GINI.summary$index / GINI.summary$delta

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Projection tools')

write.csv(GINI.summary, 'GINI_adjustment.csv')

##########################################################################################

### 2) Future Health

##########################################################################################

calc_index <- function(x, maximum, minimum, limits = T) {
  
  y <- (x - minimum) / (maximum - minimum)
  
  if(limits == T) {
  
  y <- ifelse(y > 1, 1, y)
  y <- ifelse(y < 0, 0, y)
  
  }
  
  y
  
}


##################################################################################

### Define Scenarios

##################################################################################

Health$Continent <- countrycode(Health$ISO, 'iso3c', 'continent') 
Health$WB        <- countrycode(Health$ISO, 'iso3c', 'region') 

Health$Continent <- ifelse(Health$Continent == 'Americas' & Health$WB == 'Latin America & Caribbean', 
                           'Latin America and the Caribbean', 
                           ifelse(Health$Continent == 'Americas' & Health$WB == 'North America',
                            'Northern America', Health$Continent
                            ))

Health           <- Health[complete.cases(Health), ]

### Prepare scenarios
colnames(Health_SSP_key) <- c('SCENARIO', 'Continent', substr(colnames(Health_SSP_key)[3:18], 2, 5))

for(i in 1:nrow(Health_SSP_key)) {
  
  for(j in 3:ncol(Health_SSP_key)) {
    
    Health_SSP_key[i, j] <- ifelse(Health_SSP_key$Continent[i] == 'Africa', Health_SSP_key[i, j] / Health_SSP_key[25, j], 
                              ifelse(Health_SSP_key$Continent[i] == 'Asia', Health_SSP_key[i, j] / Health_SSP_key[26, j], 
                                ifelse(Health_SSP_key$Continent[i] == 'Europe', Health_SSP_key[i, j] / Health_SSP_key[27, j], 
                                  ifelse(Health_SSP_key$Continent[i] == 'Latin America and the Caribbean', Health_SSP_key[i, j] / Health_SSP_key[28, j],
                                    ifelse(Health_SSP_key$Continent[i] == 'Northern America', Health_SSP_key[i, j] / Health_SSP_key[29, j],
                                           ifelse(Health_SSP_key$Continent[i] == 'Oceania', Health_SSP_key[i, j] / Health_SSP_key[30, j], NA))))))
    
  }
  
  print(i)
  
}

Health_SSP_key <- Health_SSP_key %>%
                    pivot_longer(colnames(Health_SSP_key)[3:18], 
                      names_to = 'Year', values_to = 'SSP Adjustment')

Health         <- merge(Health, Health_SSP_key, by = c('Continent', 'Year'))
Health$value   <- Health$value * Health$`SSP Adjustment`

Health %>% 
  mutate(index = calc_index(value, 85, 20)) %>%
   group_by(.dots = c('Year', 'SCENARIO')) %>%
    summarise(index = mean(index)) %>%
    ggplot(aes(x = Year, y = index, colour = SCENARIO)) + geom_line()


