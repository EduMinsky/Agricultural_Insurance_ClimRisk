###### Creation of final data to exploration and model ######
lapply(c('tidyverse','sf','stringi'),library,character.only = TRUE)

data_06_21 = read_rds('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\data_06_21_geom.RDS')
data_22 = read_rds('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\data_22_geom.RDS')
data_22$EVENTO_PREPONDERANTE %>% unique %>% as_tibble() %>%  view

#Separating the data into "has occured the event" and "not occurring"
not_occurring_06_21 = data_06_21 %>% 
  filter(EVENTO_PREPONDERANTE=='-' | is.na(EVENTO_PREPONDERANTE)
         | EVENTO_PREPONDERANTE=='' | EVENTO_PREPONDERANTE==' ')

occuring_06_21 = data_06_21 %>% 
  filter(EVENTO_PREPONDERANTE!='-' & !is.na(EVENTO_PREPONDERANTE)
         & EVENTO_PREPONDERANTE!='' & EVENTO_PREPONDERANTE!=' ')




not_occurring_06_21$Event = 0
occuring_06_21$Event = 1
data_22$Event = 0
not_occurring_06_21 %>% write_rds('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\not_occurring_06_21.RDS')
occuring_06_21 %>% write_rds('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\occuring_06_21.RDS')
data_22 %>% write_rds('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\not_occurring_data_22.RDS')
#As shp
not_occurring_06_21_shp = not_occurring_06_21 %>%  
  select(-`NR_DECIMAL_LATITUDE` )%>%st_as_sf() 

occuring_06_21_shp = occuring_06_21%>%  
  select(-`NR_DECIMAL_LATITUDE` )%>%st_as_sf() 

not_occurring_data_22_shp = data_22 %>%  
  select(-`NR_DECIMAL_LATITUDE` )%>%st_as_sf()

not_occurring_data_22_shp %>% st_write('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\not_occurring_data_22_shp.shp')




