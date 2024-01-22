###### Cleaning overlapping points in our dataset ######
lapply(c('tidyverse','sf','stringi'),library,character.only = TRUE)
not_occurring_data_22_shp = st_read('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\not_occurring_data_22_shp.shp')
not_occurring_06_21_shp = st_read('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\not_occurring_06_21.shp')
occuring_06_21_shp = st_read('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\occuring_06_21_shp.shp')


not_occurring_data_22_shp_unique = not_occurring_data_22_shp %>% st_difference()
not_occurring_06_21_shp_unique = not_occurring_06_21_shp %>% st_difference()
occuring_06_21_shp_unique = occuring_06_21_shp %>% st_difference()

occuring_06_21_shp_unique %>% st_write('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\occuring_06_21_shp_unique.shp')
