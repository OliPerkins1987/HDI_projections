
library(tidyverse)
library(raster)

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised/Global')

f          <- list.files(pattern = '*.csv')
dat        <- lapply(f, read.csv)
names(dat) <- substr(f, 1, nchar(f)-4)
dat        <- lapply(1:length(dat), function(i) {
  
                mutate(dat[[i]], name = names(dat)[i])
  
                  }) %>% plyr::rbind.fill()

cap         <- expand.grid(Year = seq(2020, 2100, 10), 
                           SSP = c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5'), 
                           name = unique(dat$name))
cap         <- merge(cap, dat, by = c('Year', 'SSP', 'name'), all.x = T)

### Sort out road

cap <- cap %>% 
        group_by(SSP, name) %>%
          mutate_if(is.numeric, function(x) {
            ifelse(is.na(x), x[2], x)
          })
  

cap %>% 
  group_by(name) %>% mutate(.max = max(mean, na.rm = T)) %>%
   #ungroup() %>% mutate(mean = ifelse(.max >1, mean/.max, mean))  %>%
  ggplot(aes(x= Year, y = mean, colour = SSP)) + geom_line(size= 1) +
   facet_grid(name ~., scales = 'free_y')

#############################################################################

### Calc distance

#############################################################################

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Plots/Revised')

Capitals <- pivot_wider(cap, id_cols = c(Year, SSP), 
                        names_from = name, values_from = mean) %>%
                         mutate(Metric = 'mean')

Capitals.med <- pivot_wider(cap, id_cols = c(Year, SSP), 
                            names_from = name, values_from = median) %>%
                              mutate(Metric = 'median')

Capitals <- bind_rows(list(Capitals, Capitals.med))

Capitals.norm <- Capitals %>% 
                  group_by(SSP, Metric) %>%
                   mutate_at(colnames(Capitals)[3:10], function(x) {x/x[1]})

Capitals.norm %>%
  ungroup() %>% 
  mutate(Overall = apply(Capitals.norm[, c(3:10)], 1, function(x) {mean(abs(x), na.rm = T)}), 
         `No GDP`  = apply(Capitals.norm[, c(3, 5:10)], 1, function(x) {mean(abs(x), na.rm = T)})) %>%
   pivot_longer(c(Overall, `No GDP`)) %>%  
  ggplot(aes(x = Year, y = value, colour=  SSP)) + geom_line(size=  1.25) +
     scale_colour_viridis_d() + facet_grid(Metric ~ name, scales = 'free_y') +
  theme_classic() + theme(text= element_text(size = 14)) +
   ylab('Proportional change (2020 baseline)')


Capitals.norm %>%
  filter(Year %in% c(2050, 2100) & Metric == 'mean') %>%
   mutate_at(colnames(Capitals.norm)[3:10], function(x) {(x - 1)}) %>%
   pivot_longer(colnames(Capitals.norm)[3:10]) %>%
    ggplot(aes(x = name, y = value, fill = SSP)) + 
     geom_col(position = position_dodge(), colour= 'black') +
      facet_grid(. ~ Year, scales = 'free_y') + xlab('Variable') + ylab('Proportional change (2020 baseline)') +
       theme(text = element_text(size=  14)) + scale_fill_viridis_d()


Capitals %>%
  filter(Year %in% c(2020, 2050)) %>%
  group_by(SSP) %>%
  mutate_at(colnames(Capitals)[3:10], function(x) {(x[2] - x[1])}) %>%
  pivot_longer(colnames(Capitals)[3:10]) %>%
  ggplot(aes(x = SSP, y = value, fill = SSP)) + 
  geom_col(position = position_dodge(), colour= 'black') +
  facet_grid(name ~., scales = 'free_y') + xlab('Variable') + #ylab('Proportional change (2020 baseline)') +
  theme(text = element_text(size=  14)) + scale_fill_viridis_d()




############################################################################################

### Regional

############################################################################################

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised/Regional')


dat <- list()
for(i in 1:length(list.files(pattern='*.csv'))) {
  
  dat[[i]]      <- read.csv(list.files(pattern = '*.csv')[i])

}

names(dat)            <- c('GDP', 'Gini', 'Education', 'Health', 'MA','Road', 'Energy','WAP')  
#colnames(dat[[6]])[1] <- 'WBR'
dat                   <- lapply(1:length(dat), function(i) {
                            mutate(dat[[i]], Capital = names(dat)[i])}) %>% plyr::rbind.fill()

cap         <- expand.grid(Year = seq(2020, 2100, 10), 
                           SSP = c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5'),
                           name = unique(dat$name),
                           Capital = unique(dat$Capital))

cap         <- merge(cap, dat, by = c('Year', 'SSP', 'name','Capital'))


Capitals.reg <- cap %>% filter(Year >= 2020) %>%
                 filter(var == 'mean') %>%
                        group_by(SSP, name, Capital) %>%
                          mutate(val.reg = value / value[1]) %>% ungroup()

reg.mean <- Capitals.reg %>%  
            pivot_wider(id_cols = c(Year, SSP, name), 
              names_from = Capital, values_from = val.reg) %>% rowwise() %>%
                mutate(overall =  mean(abs(c(Education, MA, Health, Energy, WAP, Road))-1, na.rm = T)) 

reg.mean %>%
  ggplot(aes(x = Year, y = overall, colour = name)) + geom_line(size=  1.25) +
  scale_colour_viridis_d() + theme_classic() + theme(text = element_text(size=  14)) +
  facet_grid(SSP~.) + ylab('Proportional change (2020 baseline)') + 
   geom_hline(yintercept = 0)# + scale_y_continuous(breaks = c(1, 1.25, 1.5, 1.75, 2))

reg.mean %>%  
  filter(Year %in% c(2100)) %>%
  filter(SSP == 'SSP5' & name == 'North America') %>%
  group_by(SSP, name) %>%
   ungroup() %>% group_by(Year, name) %>% summarise(val = mean(overall))
    
reg.mean %>% 
  filter(Year %in% c(2020, 2100)) %>%
  pivot_longer(colnames(reg.mean)[4:11], names_to = 'Capital') %>%
  group_by(SSP, name, Capital) %>%
  mutate(val = value[2] - value[1]) %>%
  ggplot(aes(x = SSP, y = val, fill = name)) + 
  geom_col(position = position_dodge(), colour= 'black') +
  facet_grid(Capital ~., scales = 'free_y') + xlab('Variable') + 
  theme_classic() +
  theme(text = element_text(size=  14)) + scale_fill_viridis_d() + 
  ylab('Proportional change (2020 baseline)')


############################################################################################

### Country

############################################################################################

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Country')

dat <- list()
for(i in 1:length(list.files(pattern='*.csv'))) {
  
  dat[[i]]      <- read.csv(list.files(pattern = '*.csv')[i])
  
}

names(dat)            <- c('Education', 'Energy', 'Gini', 'Health', 'MA','WAP') 
dat                   <- lapply(1:length(dat), function(i) {
                            mutate(dat[[i]], Capital = names(dat)[i])}) %>% bind_rows() %>%
                              mutate(SSP = toupper(SSP))

cap         <- expand.grid(Year = seq(2020, 2100, 10), 
                           SSP = c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5'),
                           Country = unique(dat$Country),
                           name = unique(dat$name))

cap         <- merge(cap, dat, by = c('Year', 'SSP', 'Country','name'))


Capitals.reg <- cap %>% filter(Year >= 2020) %>%
  filter(name == 'mean') %>%
  group_by(SSP, Country, Capital) %>%
  mutate(val.reg = value / value[1]) %>% ungroup()

reg.mean <- Capitals.reg %>%  
  pivot_wider(id_cols = c(Year, SSP, Country), 
              names_from = Capital, values_from = val.reg) %>% rowwise() %>%
  mutate(overall =  mean(abs(c(Gini, Education, MA, Health, Energy, WAP))-1, na.rm = T)) 

reg.mean %>%
  ggplot(aes(x = Year, y = overall, colour = Country)) + geom_line(size=  1.25) +
  scale_colour_viridis_d() + theme_classic() + theme(text = element_text(size=  14)) +
  facet_grid(SSP~.) + ylab('Proportional change (2020 baseline)')

reg.mean %>% 
  filter(Year %in% c(2020, 2100)) %>%
  pivot_longer(colnames(reg.mean)[4:10]) %>%
  group_by(SSP, Country, name) %>%
  mutate(val = value[2] - value[1]) %>%
  ggplot(aes(x = SSP, y = val, fill = Country)) + 
  geom_col(position = position_dodge(), colour= 'black') +
  facet_grid(name ~., scales = 'free_y') + xlab('Variable') + 
  theme(text = element_text(size=  14)) + scale_fill_viridis_d() + 
  ylab('Proportional change (2020 baseline)')

cap %>%
  filter(Year %in% c(2030, 2100)) %>%
  filter(name == 'mean') %>%
  group_by(SSP, Country, Capital) %>%
  mutate(val = value[2] - value[1]) %>%
  ggplot(aes(x = SSP, y = val, fill = Country)) + 
  geom_col(position = position_dodge(), colour= 'black') +
  facet_grid(Capital ~., scales = 'free_y') + xlab('Variable') + 
  theme(text = element_text(size=  14)) + scale_fill_viridis_d()


