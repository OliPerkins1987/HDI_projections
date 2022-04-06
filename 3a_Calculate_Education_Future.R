

library(tidyverse)
library(raster)
library(countrycode)
library(caret)

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/')

Edu     <- read.csv('SspDb_country_data_2013-06-12.csv')  

### Split data @ 25 years (as per HDI)
key     <- c('Aged0-4', 'Aged5-9', 'Aged10-14', 'Aged15-19', 'Aged20-24')

Edu.key <- lapply(key, function(x) {grepl(x, Edu$VARIABLE)})
Edu.key <- cbind.data.frame(lapply(Edu.key, data.frame))
Edu.key <- apply(Edu.key, 1, sum)

Dim.index <- function(x, maximum, minimum) {
  
  y <- (x - minimum) / (maximum - minimum)
  
  y <- ifelse(y > 1, 1, y)
  y <- ifelse(y < 0, 0, y)
  
  y
  
}

### Translate 'primary' 'secondary' 'tertiary' to number of years
Edu.weights <- c(5.2, 11.4, 15.5) #from KC et al., 2010

###################################################################################

### Education received

###################################################################################

### Calculate mean education level for people above age of 25

Education.historical            <- Edu[grepl('Aged', Edu$VARIABLE) & Edu.key == 0, ]
Education.historical$age_key    <- readr::parse_number(Education.historical$VARIABLE)
Education.historical$edu_key    <- ifelse(grepl('No Education', Education.historical$VARIABLE), 0, 
                                    ifelse(grepl('Primary', Education.historical$VARIABLE), Edu.weights[1], 
                                     ifelse(grepl('Secondary', Education.historical$VARIABLE), Edu.weights[2], 
                                      ifelse(grepl('Tertiary', Education.historical$VARIABLE), Edu.weights[3], NA))))

Education.historical <- Education.historical %>% 
                         dplyr::select(-c(6:17, 37:46)) %>%
                          pivot_longer(colnames(Education.historical)[18:36], names_to = 'Year') %>%
                            mutate(Year = as.numeric(gsub('X', '', Year))) %>% ungroup()

Education.historical <- Education.historical %>% 
                          group_by(.dots = c('REGION', 'Year', 'SCENARIO','age_key', 'edu_key')) %>%
                            summarise('Both_gender' = sum(value))

Education.historical$Total_education <- ifelse(!is.na(Education.historical$edu_key),
                                               Education.historical$Both_gender * Education.historical$edu_key,
                                                  Education.historical$Both_gender)


Education.historical <- Education.historical %>% 
                         group_by(.dots = c('REGION', 'Year', 'SCENARIO')) %>%
                           mutate(Mean_education = sum(Total_education[!is.na(edu_key)]) / sum((Total_education[is.na(edu_key)])))

Education.historical <- Education.historical %>% ungroup() %>%
                          filter(is.na(edu_key)) %>%
                            group_by(.dots = c('REGION', 'Year', 'SCENARIO')) %>%
                              summarise(Mean_education = Mean_education[1])
  

###################################################################################

### Education expected

###################################################################################

Education.future            <- Edu[Edu.key == 1, ]
Education.future$age_key    <- readr::parse_number(Education.future$VARIABLE)


Education.future$edu_key    <- ifelse(grepl('No Education', Education.future$VARIABLE), 0, 
                                     ifelse(grepl('Primary', Education.future$VARIABLE), Edu.weights[1], 
                                       ifelse(grepl('Secondary', Education.future$VARIABLE), Edu.weights[2], 
                                         ifelse(grepl('Tertiary', Education.future$VARIABLE), Edu.weights[3], NA))))

Education.future <- Education.future %>% 
                     filter(age_key == 15) %>%
                      dplyr::select(-c(6:17, 37:46)) %>%
                        pivot_longer(colnames(Education.future)[18:36], names_to = 'Year') %>%
                          mutate(Year = as.numeric(gsub('X', '', Year))) %>% ungroup()


Education.future <- Education.future %>% 
                      group_by(.dots = c('REGION', 'Year', 'SCENARIO', 'age_key', 'edu_key')) %>%
                        summarise(value = sum(value))

Education.future$Total_education <- ifelse(!is.na(Education.future$edu_key),
                                               Education.future$value * Education.future$edu_key,
                                               Education.future$value)

Education.future <- Education.future %>% 
                         group_by(.dots = c('REGION', 'Year', 'SCENARIO')) %>%
                          mutate(Mean_education = sum(Total_education[!is.na(edu_key)]) / sum((Total_education[is.na(edu_key)])))


Education.future <- Education.future %>% ungroup() %>%
                      filter(is.na(edu_key)) %>%
                        group_by(.dots = c('REGION', 'Year', 'SCENARIO')) %>%
                          summarise(Mean_education = Mean_education[1])

#####################################################################################################

### Combine, project, write out

#####################################################################################################

Education                <- merge(Education.historical, Education.future, 
                              by = c('REGION', 'Year', 'SCENARIO'), all.x = T)

colnames(Education)[4:5] <- c('Mean_schooling', 'Expected_schooling')
Education$MS_index       <- Dim.index(Education$Mean_schooling, 15, 0)
Education$FS_index       <- Dim.index(Education$Mean_schooling, 18, 0)
Education$Index          <- apply(Education[, 6:7], 1, mean)


setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Future projections/HDI/Regional HDI/Cleaned Future')

write.csv(Education, 'Education_All_SSPs.csv', row.names = F)

