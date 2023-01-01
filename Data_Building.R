path <- 'C:/Users/mfroque/OneDrive - KPMG/Documents/Dissertation/data/'
load(paste0(path, 'inclusion.list.Rdata'))
load(paste0(path, 'data_ready.Rdata'))
blocks_data <- read_sf(dsn = paste0(path, 'Census_Block_Groups_2010-shp'), 
                       layer = 'Census_Block_Groups_2010')
                       
