
library(raster)
library(rgdal)
library(tidyverse)
library(zoo)
library(smooth)
library(countrycode)

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Projections')
future.HDI <- read.csv('HDI_future_projections_v2.csv')
future.EDU <- read.csv('Education_predictions_allSSPs.csv')
future.EDU_Gini <- read.csv('Education_predictions_allSSPs_GINIcoef.csv')

future.Health      <- read.csv('Health_predictions_allSSPs.csv')
future.Health_Gini <- read.csv('Health_predictions_allSSPs_GINIcoef.csv')

#Health <- read.csv('Health_predictions_2010baseline.csv')

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Cleaned Historical')

Health_past    <- read.csv('Cleaned_Regional_Health.csv')
Education_past <- read.csv('Cleaned_Regional_Education.csv') 
GNI_past       <- read.csv('Cleaned_Regional_GNI.csv')
HDI_past       <- read.csv('Cleaned_Regional_HDI.csv')
HDI_past$HDI_nat_reg_ratio <- HDI_past$HDI / HDI_past$National_HDI

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Projection tools')
Prediction_key <-  read.csv('HDI_harmonisation_key.csv')
Prediction_key <- Prediction_key[, colnames(Prediction_key) != 'X']

######################################################################################################

### 1) Ugly data preparation

######################################################################################################


future.EDU          <- future.EDU[, c(4, 1, 3, 2, 11, 12,15)]
future.Health       <- future.Health[, c(4, 1, 2, 3, 6, 10 ,11)]

future.EDU_Gini        <- future.EDU_Gini[, c(5, 1, 3, 2, 13, 15, 16)]
future.Health_Gini     <- future.Health_Gini[, c(5, 1, 2, 3, 7, 9, 10)]

future.EDU$index_name            <- 'Education'
future.EDU_Gini$index_name       <- 'Education'
future.EDU$Downscaling_type      <- 'Fixed_weight'
future.EDU_Gini$Downscaling_type <- 'Gini'

future.Health$index_name            <- 'Health'
future.Health_Gini$index_name       <- 'Health'
future.Health$Downscaling_type      <- 'Fixed_weight'
future.Health_Gini$Downscaling_type <- 'Gini'

colnames(future.EDU)        <- c('ISO_Code', 'GDLCODE', 'SCENARIO', 'Year', 'raw_index', 'Subnat_ratio', 'Subnat_index', 'index_name', 'Downscaling_type')
colnames(future.EDU_Gini)   <- c('ISO_Code', 'GDLCODE', 'SCENARIO', 'Year', 'raw_index', 'Subnat_ratio', 'Subnat_index', 'index_name', 'Downscaling_type')
colnames(future.Health)     <- c('ISO_Code', 'GDLCODE', 'SCENARIO', 'Year', 'raw_index', 'Subnat_ratio', 'Subnat_index', 'index_name', 'Downscaling_type')
colnames(future.Health_Gini)<- c('ISO_Code', 'GDLCODE', 'SCENARIO', 'Year', 'raw_index', 'Subnat_ratio', 'Subnat_index', 'index_name', 'Downscaling_type')

future.EDU    <- rbind(future.EDU, future.EDU_Gini)
future.Health <- rbind(future.Health, future.Health_Gini)

Education_past$SCENARIO <- 'Historical'
Education_past$Downscaling_type <- 'Historical'
Education_past$index_name       <- 'Education'
Education_past                  <- Education_past[, c(1, 5, 10, 2, 8, 9, 7, 12, 11)]

colnames(Education_past)        <- colnames(future.EDU)
Education                       <- rbind(Education_past, future.EDU)


Health_past            <- Health_past[, c(1, 5, 2, 9, 8, 7)] 
Health_past$SCENARIO   <- 'Historical'
Health_past$Downscaling_type <- 'Historical'
Health_past$index_name       <- 'Health'

Health_past          <- Health_past[, c(1:2,7, 3,  5, 4, 6, 9, 8)]
colnames(Health_past)<- colnames(future.Health)
Health               <- rbind(Health_past, future.Health)

remove(future.EDU_Gini, future.Health_Gini)
gc()


#####################################################

### Harmonisation of Health

#####################################################

baseline <- 2010

test <- Health %>% filter(Downscaling_type != 'Fixed_weight') %>%
  group_by_at(.vars = c('GDLCODE', 'SCENARIO')) %>%
    mutate(Delta_downscale = c(NA, diff(Subnat_ratio))) %>%
      tidyr::fill(Delta_downscale, .direction = 'downup') %>%
        mutate(Delta_downscale = Delta_downscale / Subnat_ratio) %>%
          ungroup()

temp      <- list()
Scenarios <- unique(test$SCENARIO)

for(s in 3:length(Scenarios)) {

  print(Scenarios[s])
  
  temp[[s]] <- rbind(test[test$SCENARIO == Scenarios[s], ], 
                     test[test$SCENARIO == 'Historical', ]) 
  
  temp[[s]] <- temp[[s]][!duplicated(temp[[s]]), ]
  
for(i in 1:nrow(temp[[s]])) {
  
  if(temp[[s]]$Year[i] > baseline) {
    
    temp[[s]]$Subnat_ratio[i] <- temp[[s]]$Subnat_ratio[
                                  temp[[s]]$GDLCODE == temp[[s]]$GDLCODE[i] & temp[[s]]$Year == (
                                       temp[[s]]$Year[i] - 1)] + (temp[[s]]$Delta_downscale[i] * temp[[s]]$Subnat_ratio[
                                         temp[[s]]$GDLCODE == temp[[s]]$GDLCODE[i] & temp[[s]]$Year == (
                                           temp[[s]]$Year[i] - 1)])
    
    }
  
  print(i)
  
  }
  
  
}



temp2 <- temp[-1] %>%
          lapply(function(x) {
            
            x %>% 
              mutate(temp = ifelse(Year > 2010 & Year < 2020, NA, 
                Subnat_index)) %>%
                 group_by(GDLCODE) %>%
                  arrange(Year) %>%
                  mutate(temp = na.approx(temp, maxgap = 200, na.rm = F)) %>%
                    ungroup()
            
          })

temp2[[3]] %>% 
  group_by(Year) %>%
    summarise(value = mean(temp, na.rm = T)) %>%
      ggplot(aes(x = Year, y = value)) + geom_line()

for(i in 1:length(temp2)) {
  
  
  temp2[[i]]$SCENARIO <- paste0('SSP', i)
  
}

test <- plyr::rbind.fill(temp2)
test <- test[!duplicated(test), ]
test$Subnat_index <- test$temp


test %>% 
  group_by(SCENARIO, Year) %>%
    summarise(value = mean(Subnat_index, na.rm = T)) %>%
      ggplot(aes(x = Year, y = value, colour = SCENARIO)) + geom_line()


test <- test[, -c(9:12)]
test$Subnat_index <- ifelse(test$Subnat_index > 1, 1, test$Subnat_index)


############################################################################################

### 2) Harmonise education

############################################################################################


baseline <- 2010

test <- Education %>% filter(Downscaling_type != 'Fixed_weight') %>%
  group_by_at(.vars = c('GDLCODE', 'SCENARIO')) %>%
  mutate(Delta_downscale = c(NA, diff(Subnat_ratio))) %>%
  tidyr::fill(Delta_downscale, .direction = 'downup') %>%
  mutate(Delta_downscale = Delta_downscale / Subnat_ratio) %>%
  ungroup()

temp      <- list()
Scenarios <- unique(test$SCENARIO)

for(s in 1:length(Scenarios)) {
  
  print(Scenarios[s])
  
  temp[[s]] <- rbind(test[test$SCENARIO == Scenarios[s], ], 
                     test[test$SCENARIO == 'Historical', ]) 
  
  temp[[s]] <- temp[[s]][!duplicated(temp[[s]]), ]
  
  for(i in 1:nrow(temp[[s]])) {
    
    if(temp[[s]]$Year[i] > baseline) {
      
      temp[[s]]$Subnat_ratio[i] <- temp[[s]]$Subnat_ratio[
        temp[[s]]$GDLCODE == temp[[s]]$GDLCODE[i] & temp[[s]]$Year == (
          temp[[s]]$Year[i] - 1)] + (temp[[s]]$Delta_downscale[i] * temp[[s]]$Subnat_ratio[
            temp[[s]]$GDLCODE == temp[[s]]$GDLCODE[i] & temp[[s]]$Year == (
              temp[[s]]$Year[i] - 1)])
      
    }
    
    print(i)
    
  }
  
  
}



temp2 <- temp[-1] %>%
  lapply(function(x) {
    
    x %>% 
      mutate(temp = ifelse(Year > 2010 & Year < 2020, NA, 
                           Subnat_index)) %>%
      group_by(GDLCODE) %>%
      arrange(Year) %>%
      mutate(temp = na.approx(temp, maxgap = 200, na.rm = F)) %>%
      ungroup()
    
  })

temp2[[5]] %>% 
  group_by(Year) %>%
  summarise(value = mean(temp, na.rm = T)) %>%
  ggplot(aes(x = Year, y = value)) + geom_line()

for(i in 1:length(temp2)) {
  
  
  temp2[[i]]$SCENARIO <- paste0('SSP', i)
  
}

test <- plyr::rbind.fill(temp2)
test <- test[!duplicated(test), ]
test$Subnat_index <- test$temp


test              <- test[, -c(9:11)]
test$Subnat_index <- ifelse(test$Subnat_index > 1, 1, test$Subnat_index)

test %>% 
  group_by(SCENARIO, Year) %>%
  summarise(value = mean(Subnat_index, na.rm = T)) %>%
  ggplot(aes(x = Year, y = value, colour = SCENARIO)) + geom_line()



##############################################################

### write out

##############################################################

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Completed_data')
write.csv(test, 'Education_harmonized_2010baseline.csv', row.names = F)
write.csv(test, 'Health_harmonized_2020baseline.csv', row.names = F)





