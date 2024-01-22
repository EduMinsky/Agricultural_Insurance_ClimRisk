###### Associating the centroid with data that do not has latlong ######
lapply(c('tidyverse','sf','stringi'),library,character.only = TRUE)

#Importing dataframes
files = list.files('E:\\MBA DATA SCIENCE\\TCC\\MiddleData',full.names = T)
files = files[files%>%
                str_detect(pattern = 'NoCoord')]
data_06_21 = files[[1]]%>%read_rds()
data_22 = files[[2]]%>%read_rds()
#Importing Centroids
list.files('E:\\MBA DATA SCIENCE\\TCC\\SupportData\\Municipios')
centroid =read_sf(list.files('E:\\MBA DATA SCIENCE\\TCC\\SupportData\\Municipios',full.names = T)[[10]])
centroid = centroid %>% 
  mutate(geocode = as.numeric(CD_MUN) )#Renaming column

#Doing the inner join with geomcode
#First for 06_21
#Filtering observations without geocode!
data_06_21_nogeocode = data_06_21 %>% 
  filter(
  is.na(geocode)
)
# We need to associate 202 municipalities without geocode with the centroid database
# We are going to do some string manipulation so we can join centroid and the database

centroid = centroid %>% 
  as_tibble() %>% 
  mutate(nome_mun = stri_trans_general(str=NM_MUN,
                     id="Latin-ASCII")) %>% 
  mutate(nome_mun = str_replace_all(nome_mun, "[^[:alnum:]]", " ")) 

  
data_06_21_nogeocode = data_06_21_nogeocode %>% 
  mutate(nome_mun =stri_trans_general(str=NM_MUNICIPIO_PROPRIEDADE,
                                      id="Latin-ASCII")) %>% 
  mutate(nome_mun = str_replace_all(nome_mun, "[^[:alnum:]]", " "))

#Leftjoin those values:
leftjoin = data_06_21_nogeocode %>% left_join(centroid %>% select(nome_mun,geocode,geometry),by ='nome_mun',
                                   relationship = "many-to-many")
# After a lot of exploration, we checked for repeated values
# Those values are repeated because the municipality name were associeted with several geomcode
# that has the same name
leftjoin_repeated = leftjoin %>%
  filter(ID_PROPOSTA %in% unique(.[["ID_PROPOSTA"]][duplicated(.[["ID_PROPOSTA"]])]))
# If you take a good look at the values, you will figure that all observations has a municipality name
# but that name is not related with the state (for example, Bonito is not related with RS state)
# We are not going to use those observations

left_join_unique = leftjoin %>%
  filter(!ID_PROPOSTA %in% unique(.[["ID_PROPOSTA"]][duplicated(.[["ID_PROPOSTA"]])]),
         !is.na(geocode.y))#filtering unique values and also values without geocode
# those values without geocode is because the name of the municipality are wrong
# in fact they are the name of districs

# Now, lets associate the data that has geocode originally
data_06_21_values = data_06_21 %>% 
  filter(
    !is.na(geocode)
  )


data_06_21_values = data_06_21_values%>% left_join(centroid %>% select(nome_mun,geocode,geometry),by ='geocode',
              relationship = "many-to-many") 



data_06_21_geom = bind_rows(left_join_unique %>% rename(geocode = geocode.x) %>% select(-geocode.y),
          data_06_21_values)



#For 22_data
data_22_nogeocode = data_22%>% 
  filter(
    is.na(geocode)
  ) 


data_22_nogeocode = data_22_nogeocode%>% 
  mutate(nome_mun =stri_trans_general(str=NM_MUNICIPIO_PROPRIEDADE,
                                      id="Latin-ASCII")) %>% 
  mutate(nome_mun = str_replace_all(nome_mun, "[^[:alnum:]]", " "))
leftjoin_22 = data_22_nogeocode %>% left_join(centroid %>% select(nome_mun,geocode,geometry),by ='nome_mun',
                                              relationship = "many-to-many")


leftjoin_22_repeated = leftjoin_22 %>%
  filter(ID_PROPOSTA %in% unique(.[["ID_PROPOSTA"]][duplicated(.[["ID_PROPOSTA"]])]))

left_join_unique_22 = leftjoin_22 %>%
  filter(!ID_PROPOSTA %in% unique(.[["ID_PROPOSTA"]][duplicated(.[["ID_PROPOSTA"]])]),
         !is.na(geocode.y))


data_22_geocode = data_22%>% 
  filter(
    !is.na(geocode)
  ) 


data_22_geocode = data_22_geocode%>% left_join(centroid %>% select(nome_mun,geocode,geometry),by ='geocode',
                                 relationship = "many-to-many") 



data_22_geom = bind_rows(left_join_unique_22 %>% rename(geocode = geocode.x) %>% select(-geocode.y),
                            data_22_geocode)




data_22_geom %>% write_rds('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\data_22_geom.RDS')

data_06_21_geom%>% write_rds('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\data_06_21_geom.RDS')

