

library(tidyverse)
library(raster)
library(countrycode)
library(caret)


setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI')

Health    <- read.csv('GDL-Health-index-data.csv')
Education <- read.csv('GDL-Educational-index--data.csv') 

################################################################################

### 1) Get values by UN region

################################################################################

#######################################

### National values 

#######################################

National.Health    <- Health[Health$Level == 'National', ]
National.Education <- Education[Education$Level == 'National', ]

National.Health    <- National.Health %>% 
                        pivot_longer(colnames(National.Health)[6:35]) %>%
                          mutate(name = as.numeric(gsub('X', '', name))) %>%
                            mutate('Region' = countrycode(ISO_Code, 'iso3c','un.regionsub.name'))

National.Education <- National.Education %>% 
                        pivot_longer(colnames(National.Education)[6:35]) %>%
                          mutate(name = as.numeric(gsub('X', '', name))) %>%
                            mutate('Region' = countrycode(ISO_Code, 'iso3c','un.regionsub.name'))


National.Health$Region[National.Health$Country == 'Kosovo']       <- 'Eastern Europe'
National.Education$Region[National.Education$Country == 'Kosovo'] <- 'Eastern Europe'

### Get first year of data availability by country
National.Health    <- National.Health %>% 
                       group_by(Country) %>%
                        mutate('First_year' = min(name[!is.na(value)], na.rm = T)) %>%
                          ungroup()


National.Education <- National.Education %>% 
                        group_by(Country) %>%
                          mutate('First_year' = min(name[!is.na(value)], na.rm = T)) %>%
                            ungroup()

######################################################################

### Regional values

######################################################################

Regional.Health    <- National.Health %>% 
                        group_by(.dots = c('name', 'Region')) %>%
                          summarise('Health_index' = mean(value, na.rm = T))


Regional.Education <- National.Health %>% 
                        group_by(.dots = c('name', 'Region')) %>%
                          summarise('Education_index' = mean(value, na.rm = T))


### Regional % change
Regional.Health    <- Regional.Health %>%
                        group_by(Region) %>%
                          mutate(pct_change = lead(Health_index) / Health_index)

Regional.Education <- Regional.Education %>%
                        group_by(Region) %>%
                          mutate(pct_change = lead(Education_index) / Education_index)


################################################################################

### Back-cast countries with missing values @ the start

################################################################################

for(i in nrow(National.Health):1) {
  
  if (is.na(National.Health$value[i])) {
    
    previous  <- National.Health$value[i + 1]
    
    Region.key<- which(Regional.Health$Region == National.Health$Region[i] & Regional.Health$name == National.Health$name[i])
    pct_change<- as.numeric(Regional.Health[Region.key, 4])
    
    National.Health$value[i] <- previous / pct_change
    
    
    
  }
  
  if (is.na(National.Education$value[i])) {
    
    previous  <- National.Education$value[i + 1]
    
    Region.key<- which(Regional.Education$Region == National.Education$Region[i] & Regional.Education$name == National.Education$name[i])
    pct_change<- as.numeric(Regional.Education[Region.key, 4])
    
    National.Education$value[i] <- previous / pct_change
    
    
    
  }
  
  
  
  print(i)
  
}


######################################

### Quick fix: Syria

######################################

### syria has missing values from after the start of the tragic conflict

Syria.Education_ratio <-  National.Education$value[National.Education$Country == 'Syria'][22] / Regional.Education$Education_index[Regional.Education$Region == 'Western Asia'][22]
National.Education$value[National.Education$Country == 'Syria'][23:30] <- Regional.Education$Education_index[Regional.Education$Region == 'Western Asia'][23:30] * Syria.Education_ratio

Syria.Health_ratio    <-  National.Health$value[National.Health$Country == 'Syria'][22] / Regional.Health$Health_index[Regional.Health$Region == 'Western Asia'][22]
National.Health$value[National.Health$Country == 'Syria'][23:30] <- Regional.Health$Health_index[Regional.Health$Region == 'Western Asia'][23:30] * Syria.Health_ratio




######################################################################################

### Backcast sub-national data based on relationship with national

######################################################################################

Health <- Health %>% 
            pivot_longer(colnames(Health)[6:35], values_to = 'Health_index') %>%
              mutate(name = as.numeric(gsub('X', '', name))) %>%
                merge(National.Health[, c(2, 6,7)], by = c('ISO_Code', 'name'))

Health <- Health %>% filter(Level == 'Subnat') %>%
            mutate('Nat_reg_ratio'     = Health_index / value)

Health <- Health %>% 
          arrange(name) %>%
            arrange(GDLCODE) %>%
              arrange(Country)


for(i in nrow(Health):1) {
  
  if (is.na(Health$Health_index[i]) & Health$Region[i] == Health$Region[i+1]) {
    
    National  <- as.numeric(Health$value[i])
    ratio     <- as.numeric(Health$Nat_reg_ratio[i + 1])
    
    Health$Health_index[i]      <- National * ratio
    Health$Nat_reg_ratio[i]     <- Health$Health_index[i] / Health$value[i]
    
    
    
  }
  
  
  
  
  #print(i)
  
}


Education <- Education %>% 
  pivot_longer(colnames(Education)[6:35], values_to = 'Education_index') %>%
  mutate(name = as.numeric(gsub('X', '', name))) %>%
  merge(National.Education[, c(2, 6,7)], by = c('ISO_Code', 'name'))

Education <- Education %>% filter(Level == 'Subnat') %>%
              mutate('Nat_reg_ratio'     = Education_index / value)

Education <- Education %>% 
  arrange(name) %>%
  arrange(GDLCODE) %>%
  arrange(Country)


for(i in nrow(Education):1) {
  
  if (is.na(Education$Education_index[i]) & Education$Region[i] == Education$Region[i+1]) {
    
    National  <- as.numeric(Education$value[i])
    ratio     <- as.numeric(Education$Nat_reg_ratio[i + 1])
    
    Education$Education_index[i]      <- National * ratio
    Education$Nat_reg_ratio[i]     <- Education$Education_index[i] / Education$value[i]
    
    
    
  }

  
}



######################################

### Quick fix: Syria

######################################

### fill missing values from after the start of the conflict

for(i in 1:nrow(Health)) {
  
  if(Health$Country[i] == 'Syria') {
    
    if(is.na(Health$Health_index[i])) {
      
      Nat_reg_ratio <- (Health$Health_index[Health$Region == Health$Region[i] & Health$name == 2011])[1] / (Health$value[Health$Country == 'Syria' & Health$name == 2011])[1]
      
      Health$Health_index[i] <- Health$value[i] * Nat_reg_ratio
      
    }
    
    if(is.na(Education$Education_index[i])) {
      
      Nat_reg_ratio <- (Education$Education_index[Education$Region == Education$Region[i] & Education$name == 2011 ]) / (Education$value[Education$Country == 'Syria' & Education$name == 2011])[1]
      
      Education$Education_index[i] <- Education$value[i] * Nat_reg_ratio
      
    }
    
  }
  
}


