

library(raster)
library(tidyverse)
library(rgdal)
library(moments)

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Regional')

#wbr    <- lapply(list.files(pattern = '*.tif'), raster)[1:7]
#wbr    <- brick('WBR_degrees.tif')
wb_reg <- readOGR('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/wb_shape/World_bank_reg.shp')
wb_reg$var <- 1:7
wb_JULES <- rasterize(wb_reg, tmp, 'var')


setwd('F:/PhD/Data/Future_projections/HDI')


f <- list.files(pattern = '*.nc')
f <- f[grepl('Education', f)]
#f <- f[order(as.numeric(substr(f, nchar(f)-7, nchar(f)-4)))]
#f <- f[as.numeric(substr(f, 27, 30)) %% 10 == 0] #15, 18
#res.df <- list()
#wb.df  <- list()

for(r in 1:(length(f))) {
  
  tmp         <- brick(f[r])
  
  ### global
  
  
  res.df[[r]] <- list()
  
  for(l in c(6, 36, 86)) {
    
    res.df[[r]][[l]] <- Moran(tmp[[l]])
    
  }
  
  
  ### wb regions
  wb.df[[r]]         <- list()
  
  for(l in c(6, 36, 86)) {
    
    wb.df[[r]][[l]]  <- list()
    
    for(w in 1:7) {
    
    temp <- tmp[[l]]
    temp[wb_JULES != w]   <- NA
    temp[is.na(wb_JULES)] <- NA
    
    wb.df[[r]][[l]][[w]] <- Moran(temp)
    
    }
    
    wb.df[[r]][[l]]
    
  }
  
  
  #}
  #
  
  wb.df[[r]] <- plyr::rbind.fill(lapply(wb.df[[r]], function(x) {
                if(!is.null(x)) {
                  df <-setNames(data.frame(x), 
                           c('WB1', 'WB2', 'WB3', 'WB4', 'WB5', 'WB6', 'WB7'))
                } else {
                       
                  df <- NULL
                     }
                        })) %>%
              mutate('SSP' = paste0('SSP', r)) %>%
                pivot_longer(c('WB1', 'WB2', 'WB3', 'WB4', 'WB5', 'WB6', 'WB7'))

  write.csv(do.call(rbind, wb.df), 'Moran_region_Market.csv', row.names = F)
  
  print(r)
  
  res.df[[r]] <- lapply(res.df[[r]][!sapply(res.df[[r]], is.null)], data.frame)
  res         <- do.call(rbind, lapply(res.df, unlist))
  #write.csv(res, paste0('Road', r, '.csv'))
  
}

#"C:\Users\Oli\AppData\Local\Temp\RtmpwDQhCj\raster"
#########################################################################

### Plot

#########################################################################

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data')

Energy <- read.csv('Energy_ts.csv')

Energy %>% filter(!(SSP == 'SSP1' & Year > 2060 & Year < 2080)) %>%
ggplot(aes(x = Year, y = Sigma, colour = SSP, fill = SSP)) + geom_line(size = 1) +
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


setwd('F:/PhD/Data/Future_projections/Market_access')

Edu    <- lapply(list.files(pattern = '*.nc'), brick)#[grepl('Edu', list.files())]


wb_reg <- readOGR('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/wb_shape')
res.df <- list() 
wb.df  <- list()

for(i in 1:length(Edu)) {
  
  Edu[[i]]    <- Edu[[i]]#[[21:106]]
  Edu[[i]]    <- projectRaster(Edu[[i]], crs = crs(mask))
  plot(Edu[[i]][[1]])
  tmp         <- Edu[[i]]#( * a)
  res.df[[i]] <- list()
  wb.df[[i]]  <- list()
  
  for(j in 1:nlayers(Edu[[i]])) {
    
  res.df[[i]][[j]]      <- list()
  res.df[[i]][[j]]$mean <- mean((tmp[[j]])[], na.rm = T)
  res.df[[i]][[j]]$sd   <- sd((tmp[[j]])[], na.rm = T) 
  res.df[[i]][[j]]$mad  <- mad((tmp[[j]])[], na.rm = T)
  res.df[[i]][[j]]$median <- median(tmp[[j]][], na.rm = T)
  res.df[[i]][[j]]$skew   <- skewness(tmp[[j]][], na.rm = T)
  res.df[[i]][[j]]$q5     <- stats::quantile(tmp[[j]][], probs = 0.05, na.rm = T)
  res.df[[i]][[j]]$q95    <- stats::quantile(tmp[[j]][], probs = 0.95, na.rm = T)

  wb.df[[i]][[j]]         <- list()
  wb.df[[i]][[j]]$mean    <- t(raster::extract(tmp[[j]], wb_reg, fun = mean, na.rm = T))
  wb.df[[i]][[j]]$mad     <- t(raster::extract(tmp[[j]], wb_reg, fun = mad, na.rm = T))
  wb.df[[i]][[j]]$q       <- t(raster::extract(tmp[[j]], wb_reg, fun = quantile, na.rm = T))
  
  print(j)
  
  }
  
  print(i)
  
}


### global stats

Edu.df <- lapply(res.df, function(x) {
  
  do.call(rbind, lapply(x, function(y) {data.frame(y)}))
})

for(i in 1:length(Edu.df)) {
  
  Edu.df[[i]]$SSP <- paste0('SSP', i)
  
}

Edu.df <- do.call(rbind, Edu.df)

Edu.df %>% mutate(Year = rep(2015:2100, times = 5)) %>%
  ggplot(aes(x = Year, y = mean, colour = SSP, fill = SSP)) + geom_line(size = 1) +
  theme_classic() + scale_colour_viridis_d() + scale_fill_viridis_d() +
  theme(text = element_text(size = 26))

Edu.df %>% mutate(Year = rep(2015:2100, times = 5)) %>%
  ggplot(aes(x = Year, y = mad, colour = SSP, fill = SSP)) + geom_line(size = 1) +
  theme_classic() + scale_colour_viridis_d() + scale_fill_viridis_d() +
  theme(text = element_text(size = 26))

Edu.df %>% mutate(Year = rep(2015:2100, times = 5)) %>%
  ggplot(aes(x = Year, y = q95, colour = SSP, fill = SSP)) + geom_line(size = 1) +
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
      tmp     <- pivot_longer(tmp, colnames(tmp)[1:7])
      
      m1      <- setNames(data.frame(x[[1]]), wb_reg$REGION_WB)
      m1$var  <- 'mean'
      m1      <- pivot_longer(m1, colnames(m1)[1:7])
      
      m2      <- setNames(data.frame(x[[2]]), wb_reg$REGION_WB)
      m2$var  <- 'mad'
      m2      <- pivot_longer(m2, colnames(m2)[1:7])
      
      rbind(m1, m2, tmp)
      
    })
    
  }
  

test <- do.call(rbind, lapply(1:5, function(i) {mutate(plyr::rbind.fill(test[[i]]), 
                    SSP = paste0('SSP', i))}))

test$Year <- rep(rep(2015:2100, each = 49), times=  5)

test %>% filter(var == 'mean') %>%
ggplot(aes(x = Year, y = value, colour = name)) + geom_line() +
  facet_grid(SSP~.)


