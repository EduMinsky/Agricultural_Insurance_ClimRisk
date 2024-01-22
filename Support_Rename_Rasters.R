library(terra)
library(raster)
library(purrr)
# Importando todos os arquivos rasters:

var_files <- list.files('./data_clim/Variables',pattern = '.tif$',full.names = T)
var_rast = map(.x = var_files, .f=terra::rast)
#Renomeando:
names(var_rast[[1]]) <- 'anu_mean_temp_anomaly'

names(var_rast[[2]]) <- 'anu_precipitation_anomaly'

names(var_rast[[3]]) <- c('bio1','bio2','bio3','bio4','bio5','bio6',
'bio7','bio8','bio9','bio10','bio11','bio12','bio13','bio14','bio15','bio16','bio17','bio18','bio19')

names(var_rast[[4]]) <- 'terrain ruggedness index'

#Stackando
stack_var <- c(var_rast[[1]],var_rast[[2]],var_rast[[3]],var_rast[[4]])
#Salvando:
terra::writeRaster(stack_var, './data_clim/Variables/stack_raster.tif')
