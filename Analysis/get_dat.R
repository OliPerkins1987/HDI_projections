
library(RCurl)
library(raster)


root <- 'https://thredds.imk-ifu.kit.edu/thredds/fileServer/luc/GlobalSSP/Public/'
dirs <- c('GINI_new025/')
fn   <- c('giniratio')

### download dir
setwd('C:/Users/Oli/Desktop/Gini_temp')

for(i in 1:length(dirs)) {
  
  u <- paste0(root, dirs[i], fn[i]) #
  
    for(s in c('ssp1', 'ssp2', 'ssp3', 'ssp4', 'ssp5')) {
      
      us <- paste0(u, s)
      
      for(y in c(seq(2020, 2090, 10), 2099)) {
        
        usy <- paste0(us,'y', y, '.tif')
        
        download.file(usy, 
                      paste0(fn[i], s, y, '.tif'), mode = 'wb')
        
        
      }
      
      
      
    }

  
}



