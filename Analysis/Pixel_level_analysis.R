
library(raster)
library(tidyverse)

### load data

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised/New GDP')
GDP <- lapply(list.files(), brick)

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised/New HDI')
Health <- lapply(list.files()[grepl( 'Health',list.files())], brick)
Edu    <- lapply(list.files(pattern = 'Education.*'), brick)

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised/New MA')
MA <- lapply(list.files(pattern = '*.nc')[1:5], brick)

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised/New road')
Road <- lapply(list.files(pattern = '*.tif'), brick)

for(i in 1:5) {
  
  Road[[i]] <- projectRaster(Road[[i]], crs = crs(Health[[1]]))
  extent(Road[[i]]) <- c(-180, 180, -60.99624, 84.38276)
  Road[[i]] <- resample(Road[[i]], Health[[1]])
  print(i)
  
}

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised/New TES')
TES <- lapply(list.files(pattern = '*.tif'), brick)

for(i in 1:5) {
  
  TES[[i]] <- projectRaster(TES[[i]], crs = crs(Health[[1]]))
  extent(TES[[i]]) <- c(-180, 180, -57.30633, 85.11867)
  TES[[i]] <- resample(TES[[i]], Health[[1]])
  print(i)
  
}


setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised/New WAP')
WAP <- lapply(list.files(pattern = '*.tif'), brick)

for(i in 1:5) {
  
  WAP[[i]]         <- projectRaster(WAP[[i]], crs = crs(Health[[1]]))
  extent(WAP[[i]]) <- c(-180, 180, -57.30633, 85.11867)
  WAP[[i]]         <- resample(WAP[[i]], Health[[1]])
  print(i)
  
}

setwd('C:/Users/Oli/Documents/PhD/Journal Submissions/Forcing_data/Data/Revised/New Gini')

Gini <- lapply(list.files(pattern = '*.tif'), brick)

for(i in 1:5) {
  
  Gini[[i]]         <- projectRaster(Gini[[i]], crs = crs(Health[[1]]))
  extent(Gini[[i]]) <- c(-180, 180, -57.07574, 76.48726)
  Gini[[i]]         <- resample(Gini[[i]], Health[[1]])
  print(i)
  
}

######################################################################################################

## make unified index

######################################################################################################

key <- c('Edu', 'GDP', 'Gini', 'Health', 'MA', 'Road', 'TES', 'WAP')

for(k in key) {
  
  tmp       <- get(k)
  assign(paste0(k, '_baseline'), brick(lapply(tmp, function (z) z[[1]])))
  
  tmp       <- lapply(tmp, function(z) {(z[[4]])})
  
  .mean <- mean(sapply(tmp, function(z) {mean(z[], na.rm = T)}))
  .sd   <- mean(sapply(tmp, function(z) {sd(z[], na.rm = T)}))
  
  assign(paste0(k, '_diff'), tmp)
  
  for(l in 1:length(tmp)) {
    
    tmp[[l]] <- abs(tmp[[l]] -.mean)/.sd
  }
  
  assign(paste0(k, '_zscore'), tmp)
  
  print(k)
  
}

baseline <- list(Edu_baseline, 
                 GDP_baseline,
                 Gini_baseline,
                 Health_baseline,
                 MA_baseline,
                 Road_baseline,
                 TES_baseline,
                 WAP_baseline)

### Group
diff <-list(Edu_diff, 
            GDP_diff,
            Gini_diff,
            Health_diff,
            MA_diff,
            Road_diff,
            TES_diff,
            WAP_diff)


### make index
Edu_diff.all <- max(brick(Edu_diff)) - min(brick(Edu_diff))
GDP_diff.all <- max(brick(GDP_diff)) - min(brick(GDP_diff))
Gini_diff.all<- max(brick(Gini_diff)) - min(brick(Gini_diff))
Health_diff.all <- max(brick(Health_diff)) - min(brick(Health_diff))
MA_diff.all     <- max(brick(MA_diff)) - min(brick(MA_diff))
Road_diff.all   <- max(brick(Road_diff)) - min(brick(Road_diff))
TES_diff.all    <- max(brick(TES_diff)) - min(brick(TES_diff))
WAP_diff.all    <- max(brick(WAP_diff)) - min(brick(WAP_diff))

all.diff <- list(Edu_diff.all, 
                 GDP_diff.all,
                 Gini_diff.all,
                 Health_diff.all,
                 MA_diff.all,
                 Road_diff.all,
                 TES_diff.all,
                 WAP_diff.all)

all.diff             <- lapply(all.diff, abs)

### scale non-scaled indices
for(i in c(2,6:8)) {

all.diff[[i]] <- all.diff[[i]] / max(max(brick(diff[[i]]))[], na.rm = T)

}

### plot
plot(mean((brick(all.diff))))
png('Capitals_2050_map.png', height=nrow(all.diff[[1]]), width=ncol(all.diff[[1]])) 
plot(mean((brick(all.diff))), maxpixels=ncell(brick(all.diff)))
dev.off()




#######################################################################

### Clustering

#######################################################################

diff.cluster <- setNames(as.data.frame(brick(all.diff)), 
                         c('Education', 'GDP', 'Gini', 'Health', 
                           'Market', 'Road', 'TES', 'WAP'))
diff.cluster <- diff.cluster[complete.cases(diff.cluster), ]
diff.mod     <- princomp(diff.cluster)

set.seed(1987)
wss<- NULL
for (i in 1:10) {
  fit = kmeans(diff.mod$scores,centers = i)
  wss = c(wss, fit$tot.withinss)
}
plot(1:10, wss, type = "o")

set.seed(1987)
fit <- kmeans(diff.mod$scores, 4, iter.max = 1000, nstart = 100)

cluster.rast <- mean((brick(all.diff)))
cluster.rast[!is.na(cluster.rast)] <- fit$cluster

plot(cluster.rast, col = c(
  '#440154FF', '#238A8DFF', '#55C667FF', '#FDE725FF'
))


### Capitals by cluster

diff.cluster$cluster <- fit$cluster

diff.cluster %>% 
  group_by(cluster) %>%
   summarise_if(is.numeric, mean)

apply(diff.cluster %>% 
        group_by(cluster) %>%
        summarise_if(is.numeric, mean), 2, function(z) {z/mean(z)})

### How does this relate to 2020?

baseline.cluster     <- setNames(as.data.frame(brick(lapply(baseline, function(x) {mean((x))}))), 
                                 c('Education', 'GDP', 'Gini', 'Health', 
                                   'Market', 'Road', 'TES', 'WAP'))
baseline.cluster         <- baseline.cluster[complete.cases(baseline.cluster), ]
baseline.cluster$cluster <- NA
.match                   <- match(rownames(baseline.cluster), names(fit$cluster))

baseline.cluster$cluster[
  match(names(fit$cluster), rownames(baseline.cluster))
] <- fit$cluster[.match[!is.na(.match)]]

baseline.cluster %>% 
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise_if(is.numeric, mean)

apply(baseline.cluster %>% 
        filter(!is.na(cluster)) %>%
        group_by(cluster) %>%
        summarise_if(is.numeric, mean), 2, function(z) {z/mean(z)})

### how much has this changed?

is.thesame <- data.frame(rbind(apply(diff.cluster %>% 
                            group_by(cluster) %>%
                            summarise_if(is.numeric, mean), 2, function(z) {z/mean(z)}), 
                    apply(baseline.cluster %>% 
                            filter(!is.na(cluster)) %>%
                            group_by(cluster) %>%
                            summarise_if(is.numeric, mean), 2, function(z) {z/mean(z)})))

is.thesame$cluster <- as.character(rep(1:4), times = 2)
is.thesame$Year    <- as.factor(rep(c(2020, 2100), each = 4))

is.thesame %>%
  pivot_longer(2:9) %>%
   ggplot(aes(x = cluster, y = value -1, fill = Year)) + 
     geom_col(position = position_dodge(), colour = 'black') +
      facet_grid(name ~.) + scale_fill_viridis_d() +
       ylab('Normalised indicator value') + theme_classic() +
        geom_hline(yintercept = 0) + theme(text = element_text(size = 14))


##############################################

### What if we cluster 2020?

##############################################

baseline.mod     <- princomp(apply(baseline.cluster[, -9], 2, function(z) {z/max(z)}))

set.seed(1987)
wss<- NULL
for (i in 1:5) {
  fit2 = kmeans(baseline.mod$scores,centers = i)
  wss = c(wss, fit2$tot.withinss)
}
plot(1:5, wss, type = "o")

set.seed(1987)
fit2 <- kmeans(baseline.mod$scores, 4, iter.max = 1000, nstart = 100)

cluster.rast2 <- mean(brick(lapply(baseline, function(x) {mean((x))})))
cluster.rast2[!is.na(cluster.rast2)] <- fit2$cluster

plot(cluster.rast2, col = c(
  '#440154FF', '#238A8DFF', '#55C667FF', '#FDE725FF'
))

plot(brick(list(cluster.rast2, cluster.rast)), 
     col = c(
       '#440154FF', '#238A8DFF', '#55C667FF', '#FDE725FF'
     ))

table(as.data.frame(brick(list(cluster.rast, cluster.rast2)))) / 
  matrix(rep(table(diff.cluster$cluster), times = 4), nrow = 4) * 100

### reassign for convenience

values(cluster.rast2) <- ifelse(values(cluster.rast2) == 4, 1, 
                  ifelse(values(cluster.rast2) == 1, 4, 
                   values(cluster.rast2)))

plot(brick(list(cluster.rast2, cluster.rast)), 
     col = c(
       '#440154FF', '#238A8DFF', '#55C667FF', '#FDE725FF'
     ))


table(as.data.frame(brick(list(cluster.rast2, cluster.rast)))) /
  matrix(rep(table(ifelse(fit2$cluster == 4, 1, ifelse(fit2$cluster == 1, 4, fit2$cluster))), times = 4), nrow = 4)

####################
### Paper plot of raster
####################

cluster.plot <- as.data.frame(brick(list(
  '2020_values' = cluster.rast2, '2100_values' = cluster.rast)
), xy = T)

cluster.plot %>% 
  pivot_longer(3:4, values_to = 'cluster') %>%
  mutate(cluster = factor(cluster)) %>%
  mutate(name = recode(name, 'X2020_values' = '2020', 
                       'X2100_values' = '2100')) %>%
  #filter(!is.na(cluster)) %>%
   ggplot(aes(x = x, y = y, fill = cluster)) +
    geom_raster() + 
  scale_fill_viridis_d(na.value = 'white') + 
  #scale_y_continuous(limits = c(-60, 85)) +
  ylab('') + xlab('') + facet_grid(name~.)+
  #theme_classic() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank(), 
        text = element_text(size = 14)) +
  theme(legend.position="bottom")



