

library(raster)
library(tidyverse)
library(rgdal)
library(moments)

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Regional')

wbr    <- lapply(list.files()[8:14], raster)
#wbr    <- lapply(wbr, function(z) {resample(z, tmp)})

#for(i in 1:nlayers(wbr)) {wbr[[i]][wbr[[i]] == i] <- 1}
wb_reg <- readOGR('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/wb_shape/World_bank_reg.shp')
wb_reg <- spTransform(wb_reg, crs(tmp))

setwd('C:/Users/Oli/Desktop/Gini_temp')

f <- list.files(pattern = '*.tif')
f <- f[order(as.numeric(substr(f, nchar(f)-7, nchar(f)-4)))]
#f <- f[as.numeric(substr(f, 27, 30)) %% 10 == 0] #27, 30

for(ssp in c('ssp1', 'ss2', 'ssp3', 'ssp4', 'ssp5')) {

  res.df <- list()
  wb.df  <- list()
  gc()
  f <- files[grepl(ssp, tolower(files))]
  
  .rast <- brick(lapply(f, raster))
  writeRaster(.rast, paste0('Gini_', ssp, '.tif'), overwrite = T)
  
}
  
for(r in 1:(length(f))) {
  
  tmp         <- raster(f[r])
  
  ### global
  res.df[[r]] <- list()
  
  res.df[[r]]$mean   <- mean(tmp[], na.rm = T)
  res.df[[r]]$sd     <- sd(tmp[], na.rm = T)
  res.df[[r]]$mad    <- mad(tmp[], na.rm = T)
  res.df[[r]]$median <- median(tmp[], na.rm = T)
  res.df[[r]]$skew   <- skewness(tmp[], na.rm = T)
  res.df[[r]]$q5     <- stats::quantile(tmp[], probs = 0.05, na.rm = T)
  res.df[[r]]$q95    <- stats::quantile(tmp[], probs = 0.95, na.rm = T)
    
  ### wb regions
  wb.df[[r]]         <- list()
  
  for(a in 1:length(wbr)) {
  
    wb.df[[r]][[a]]  <- list()
    
    wb.df[[r]][[a]]$mean    <-  t(raster::extract(tmp, wb_reg, fun = mean, na.rm = T))
    wb.df[[r]][[a]]$mad     <-  t(raster::extract(tmp, wb_reg, fun = mad, na.rm = T))
    wb.df[[r]][[a]]$q       <-  t(raster::extract(tmp, wb_reg, fun = quantile, na.rm = T))
  
    gc()
    
    print(a)
  
    removeTmpFiles(h = 0.15)
    
    #print(r, ' region is complete')
    
  }
  
  teg <- plyr::rbind.fill(lapply(wb.df[[r]], data.frame)) %>%
            mutate('WBR' = rep(wb_reg$REGION_WB, each = 5)) %>%
             pivot_longer(c('mean', 'mad', 'q'))
  
  teg <- teg[!duplicated(teg), ]
  
  wb.df[[r]] <- teg
  
  write.csv(do.call(rbind, wb.df), paste0('Gini_025', r, '.csv'), row.names = F)
  
  print(r)
  
  res <- do.call(rbind, lapply(res.df, data.frame))
  write.csv(res, paste0(ssp, '_Fini_025_', r, '.csv'))
  
  }
  
}

#"C:\Users\Oli\AppData\Local\Temp\RtmpwDQhCj\raster"
#########################################################################

### Plot

#########################################################################

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised')

Energy <- read.csv('Gini.csv')

Energy %>%
ggplot(aes(x = Year, y = mean, colour = SSP, fill = SSP)) + geom_line(size = 1) +
  theme_classic() + scale_colour_viridis_d() + scale_fill_viridis_d() +
  theme(text = element_text(size = 26))


setwd('F:/PhD/Data/Future_projections/Working_age_pop')

wap <- lapply(list.files(pattern = '*.csv'), read.csv) 

for(i in 1:length(wap)) {
  
  wap[[i]]$SSP <- paste0('SSP', i)
  
}

do.call(rbind, wap) %>%
  filter(!(SSP == 'SSP5' & Year > 2040 & Year < 2060)) %>%
  filter(!(SSP == 'SSP4' & Year > 2060 & Year < 2080)) %>%
ggplot(aes(x = Year, y = Sigma, colour = SSP, fill = SSP)) + geom_line(size = 1.5) +
  theme_classic() + scale_colour_viridis_d() + scale_fill_viridis_d() +
  theme(text = element_text(size = 26))



setwd('F:/PhD/Data/Future_projections/GINI/')
root <- getwd()
res  <- list()


for(i in 1:5) {
  
setwd(paste0(root, '/SSP', i))
  
res[[i]] <- read.csv('Gini.csv')
res[[i]]$SSP <- paste0('SSP', i)

  
}



do.call(rbind, res) %>% 
  mutate(Year = rep(2015:2100, times = 5)) %>%
  filter(Year != 2100) %>%
  ggplot(aes(x = Year, y = mean, colour = SSP, fill = SSP)) + geom_line(size = 1) +
    theme_classic() + scale_colour_viridis_d() + scale_fill_viridis_d() +
     theme(text = element_text(size = 26))

do.call(rbind, res) %>% 
  mutate(Year = rep(2015:2100, times = 5)) %>%
  filter(Year != 2100) %>%
  ggplot(aes(x = Year, y = mad, colour = SSP, fill = SSP)) + geom_line(size = 1) +
  theme_classic() + scale_colour_viridis_d() + scale_fill_viridis_d() + 
  theme(text = element_text(size = 26))


########################################################################################

### HDI 

########################################################################################

setwd('F:/PhD/Data/Future_projections/GINI/SSP1')
mask <- brick(list.files(pattern = '*.tif')[1])
#mask <- raster(ncol = 360/0.25, nrow = 180/0.25)
#crs(mask) <- crs(wb_reg)

wb_reg <- readOGR('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/wb_shape/World_bank_reg.shp')
wb_reg <- spTransform(wb_reg, crs(Edu[[1]]))

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised/New HDI')
Edu    <- lapply(list.files(pattern = '*.nc'), brick)
#Edu    <- lapply(Edu, function(z) {brick(z[[c(1, seq(6, 86, 10))]])})
#Edu.weight <- t(raster::extract(area(Edu[[1]]), wb_reg, fun = mean, na.rm = T))

setwd('C:/Users/Oli/Documents/PhD/Model development/Data/Processed secondary/Merged with JULES')
JULES.mask         <- raster('JULES_Mask.tif')
extent(JULES.mask) <- c(-180, 180, -90, 90)

JULES.mask <- crop(JULES.mask, Edu[[1]])
JULES.mask <- resample(JULES.mask, Edu[[1]])
JULES.mask[JULES.mask <0 ] <- 0

res.df <- list() 
wb.df  <- list()

#wb_reg <- rgdal::readOGR('C:/Users/Oli/Documents/PhD/Model development/Data/Secondary data/World_Countries')
#wb_reg <- wb_reg[wb_reg$COUNTRY %in% c('Bangladesh', 'China', 'India', 
#          'Russian Federation', 'Mexico', 'Pakistan', 'Indonesia', 'Brazil', 'Nigeria', 'United States'), ]

for(i in 1:length(Edu)) {
  
  tmp         <- Edu[[i]]
  res.df[[i]] <- list()
  #wb.df[[i]]  <- list()
  
  for(j in 1:nlayers(Edu[[i]])) {

  res.df[[i]][[j]]      <- list()
  res.df[[i]][[j]]$mean <- mean((tmp[[j]])[], na.rm = T)
  res.df[[i]][[j]]$sd   <- sd((tmp[[j]])[], na.rm = T) 
  res.df[[i]][[j]]$mad  <- mad((tmp[[j]])[], na.rm = T)
  res.df[[i]][[j]]$median <- median((tmp[[j]])[], na.rm = T)
  res.df[[i]][[j]]$skew   <- moments::skewness((tmp[[j]])[], na.rm = T)
  res.df[[i]][[j]]$q5     <- stats::quantile((tmp[[j]])[], probs = 0.05, na.rm = T)
  res.df[[i]][[j]]$q95    <- stats::quantile((tmp[[j]])[], probs = 0.95, na.rm = T)
  
  
  #wb.df[[i]][[j]]         <- list()
  #wb.df[[i]][[j]]$mean    <- t(raster::extract(tmp[[j]], wb_reg, fun = mean, na.rm = T))
  #wb.df[[i]][[j]]$mad     <- t(raster::extract(tmp[[j]], wb_reg, fun = mad, na.rm = T))
  #wb.df[[i]][[j]]$q       <- t(raster::extract(tmp[[j]], wb_reg, fun = quantile, na.rm = T))

  print(j)
  
  }
  
  print(i)
  
}


### global stats

Edu.df <- lapply(res.df[-c(1:5)], function(x) {
  
  do.call(rbind, lapply(x, function(y) {data.frame(y)}))
})

for(i in 1:length(Edu.df)) {
  
  Edu.df[[i]]$SSP <- paste0('SSP', i)
  
}

Edu.df <- do.call(rbind, Edu.df)

Edu.df %>% mutate(Year = rep((c(2015, seq(2020, 2100,10))), times = 5)) %>%
  ggplot(aes(x = Year, y = mean, colour = SSP, fill = SSP)) + geom_line(size = 1) +
  theme_classic() + scale_colour_viridis_d() + scale_fill_viridis_d() +
  theme(text = element_text(size = 26))

Edu.df %>% mutate(Year = rep(seq(2020, 2100, 10), times = 5)) %>%
  ggplot(aes(x = Year, y = mad, colour = SSP, fill = SSP)) + geom_line(size = 1) +
  theme_classic() + scale_colour_viridis_d() + scale_fill_viridis_d() +
  theme(text = element_text(size = 26))

Edu.df %>% mutate(Year = rep(seq(2020, 2100, 10), times = 5)) %>%
  ggplot(aes(x = Year, y = q95, colour = SSP, fill = SSP)) + geom_line(size = 1) +
  theme_classic() + scale_colour_viridis_d() + scale_fill_viridis_d() +
  theme(text = element_text(size = 26))


Edu.df %>% mutate(Year = rep(seq(2020, 2100, 10), times = 5)) %>%
  ggplot(aes(x = Year, y = sd, colour = SSP, fill = SSP)) + geom_line(size = 1) +
  theme_classic() + scale_colour_viridis_d() + scale_fill_viridis_d() +
  theme(text = element_text(size = 26))

### wb stats
setNames(data.frame(c(1:5)), 'teg')


test<- list()

for(i in 1:length(wb.df)) {
  
  test[[i]]        <- lapply(1:length(wb.df[[i]]), list)
  names(wb.df[[i]])<- wb_reg$REGION_WB
  
    test[[i]] <- lapply(wb.df[[i]], function(x) {
      
      tmp     <- setNames(data.frame(x[[3]]), wb_reg$REGION_WB)
      tmp$var <- c('q0', 'q25', 'q50', 'q75','q100')
      tmp     <- pivot_longer(tmp, colnames(tmp)[1:length(wb_reg$REGION_WB)])
      
      m1      <- setNames(data.frame(x[[1]]), wb_reg$REGION_WB)
      m1$var  <- 'mean'
      m1      <- pivot_longer(m1, colnames(m1)[1:length(wb_reg$REGION_WB)])
      
      m2      <- setNames(data.frame(x[[2]]), wb_reg$REGION_WB)
      m2$var  <- 'mad'
      m2      <- pivot_longer(m2, colnames(m2)[1:length(wb_reg$REGION_WB)])
      
      rbind(m1, m2, tmp)
      
    })
    
  }
  

test <- do.call(rbind, lapply(1:5, function(i) {mutate(plyr::rbind.fill(test[[i]]), 
                    SSP = paste0('SSP', i))}))

test$Year <- rep(rep((seq(2020, 2100,10)), each = 49), times=  5)

test %>% filter(var == 'mean') %>%
ggplot(aes(x = Year, y = value, colour = name)) + geom_line() +
  facet_grid(SSP~.)


setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised/Regional')

write.csv(test, 'Gini.csv', row.names = F)


############################################################################################

### Country

############################################################################################

setwd('F:/PhD/Data/Future_projections/GINI/SSP1')
mask      <- raster(list.files(pattern = '*.tif')[1])


setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Country')
country.m <- lapply(list.files(pattern = '*.tif'), raster)


#######################
### run
#######################


setwd('F:/PhD/Data/Unprocessed secondary/Roads')

f <- list.files(pattern = '*.tif')
f <- f[order(as.numeric(substr(f, nchar(f)-7, nchar(f)-4)))]
f <- f[as.numeric(substr(f, 27, 30)) %in% c(2030, 2050, 2100)] #27, 30
res.df <- list()
wb.df  <- list()

for(r in 1:(length(f))) {
  
  tmp         <- raster(f[r])

  ### wb regions
  wb.df[[r]]         <- list()
  
  for(a in 1:length(country.m)) {
    
    wb.df[[r]][[a]]  <- list()
    
    wb.df[[r]][[a]]$mean    <- mean((wbr[[a]] * tmp)[], na.rm = T)
    wb.df[[r]][[a]]$mad     <- mad((wbr[[a]] * tmp)[], na.rm = T)
    wb.df[[r]][[a]]$q       <- quantile((wbr[[a]] * tmp)[], na.rm = T)
    
    gc()
    
    print(a)
    
    removeTmpFiles(h = 0.15)

  }
  
  teg <- plyr::rbind.fill(lapply(wb.df[[r]], data.frame)) %>%
    mutate('WBR' = rep(wb_reg$REGION_WB, each = 5)) %>%
    pivot_longer(c('mean', 'mad', 'q'))
  
  teg <- teg[!duplicated(teg), ]
  
  wb.df[[r]] <- teg
  
  write.csv(do.call(rbind, wb.df), paste0('Road_region', r, '.csv'), row.names = F)

}


#############################################################################

### country

#############################################################################

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Country/WAP')

GINI <- lapply(list.files(pattern = '*.csv'), read.csv)
for(i in 1:5) {GINI[[i]]$SSP <- paste0('ssp', i)
              GINI[[i]]$Year <- GINI[[1]]$Year
              }

GINI <- bind_rows(GINI)
GINI$Country <- substr(GINI$Country, 1, nchar(GINI$Country)-4)
  
  