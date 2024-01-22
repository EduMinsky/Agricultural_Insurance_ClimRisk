##### This script is to explore the problems with geographical information #####

###### Loading Data and packages ######
lapply(c('tidyverse','sf'),library,character.only = TRUE)
data_06_21 = read_rds('./MiddleData/data_clean_06_21.rds')
data_22 = read_rds('./MiddleData/data_clean_22.rds')



###### Dealing with those wrong infos about Latitude Longitude ######
# First, let's filter all NA values
data_06_21 %>% 
  filter(is.na(LATITUDE)|is.na(LONGITUDE))# Zero lines. So, every line has value
#Now, lets filter all LAT == S AND LONG == W
data_S_W_coord = data_06_21 %>% 
  filter(LATITUDE == 'S' & LONGITUDE =='W'  )

#Now, we filter all data that has an empty string as value
data_empty_LATLONG = data_06_21 %>% 
  filter(LATITUDE=='' | LONGITUDE=='')
#Now we filter all data that has Lat==N and Long == W
data_N_W_coord = data_06_21 %>% 
  filter(LATITUDE=='N' & LONGITUDE=='W')

#Now we filter all data that has LAT==N and LONG ==E
data_N_E_coord = data_06_21 %>% 
  filter(LATITUDE=='N' & LONGITUDE=='E')

#Now we filter all data that has LAT == S and Long == E
data_S_E_coord = data_06_21 %>% 
  filter(LATITUDE=='S' & LONGITUDE=='E')

#check if, while filtering all values, we have duplicated values:
inner_join(x = data_S_E_coord,y =data_N_E_coord,by = 'ID_PROPOSTA' )%>%
  inner_join(.,y = data_N_W_coord,by = 'ID_PROPOSTA')%>%
  inner_join(.,y =data_empty_LATLONG,by = 'ID_PROPOSTA')%>%
  inner_join(.,y =data_S_W_coord,by='ID_PROPOSTA' )# Dataframe with 0 lines
#It means that every dataframe is unique!!

#Merge all good data:
good_data = bind_rows(data_S_W_coord,data_N_W_coord)
#Merge all Bad Data:
bad_data = bind_rows(data_empty_LATLONG,data_N_E_coord,data_S_E_coord)


#Transforming the lat long to decimal degree, only for the good data
good_data_coords =good_data %>%
  mutate(lat_dec=
           case_when(LATITUDE=='N'~ +(NR_GRAU_LAT + (NR_MIN_LAT/60) + (NR_SEG_LAT/60^2)),
                     LATITUDE=='S'~ - (NR_GRAU_LAT + (NR_MIN_LAT/60) + (NR_SEG_LAT/60^2))
                     ),
         long_dec =-(NR_GRAU_LONG+(NR_MIN_LONG/60)+(NR_SEG_LONG/60^2))) %>% 
  select(-NR_GRAU_LAT,-NR_MIN_LAT,-NR_SEG_LAT,-NR_GRAU_LONG,-NR_MIN_LONG,
         -NR_SEG_LONG)



#filtering all lat long that has NA
good_data_no_coord = good_data_coords %>% 
  filter(is.na(lat_dec) | is.na(long_dec) ) %>% as_tibble


good_data_coord = good_data_coords %>% 
  filter( !is.na(lat_dec) &
            !is.na(long_dec) ) %>% as_tibble

#Merging the good data no coord with bad data
bad_data2 = bind_rows(good_data_no_coord %>% select(-lat_dec,-long_dec),bad_data)

#Checking number of rows:
bad_data2 %>% nrow+good_data_coord %>% nrow == data_06_21 %>% nrow#TRUE
#Saving those dataframes:
good_data_coord %>% 
  write_rds(
    file = 'E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\06_21_goodDataCoord.rds')
bad_data2 %>% 
  write_rds('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\06_21_badDataNoCoord.rds')

#Working with the data_22 data:
#Filtering data that has empty info about long or lat
data_22_noCoord = data_22 %>% 
  filter(LATITUDE=='-' | LONGITUDE=='-') 
#Filtering data that has info about long or lat
data_22_Coord = data_22 %>% 
  filter(LATITUDE!='-' | LONGITUDE!='-') 
#checking if all data is filtered:
data_22_noCoord %>% nrow + data_22_Coord %>% nrow == data_22 %>% nrow#TRUE

#Transforming the lat long to decimal degree, only for the good data
#but first we have to transform from char to numeric
data_22_Coord =data_22_Coord %>% mutate(
  NR_GRAU_LAT = NR_GRAU_LAT %>% as.numeric(),
  NR_MIN_LAT = NR_MIN_LAT %>% as.numeric(),
  NR_SEG_LAT = NR_SEG_LAT %>% as.numeric(),
  NR_GRAU_LONG = NR_GRAU_LONG %>% as.numeric(),
  NR_MIN_LONG = NR_MIN_LONG %>% as.numeric(),
  NR_SEG_LONG = NR_SEG_LONG %>% as.numeric()
) %>% 
  mutate(lat_dec=
           case_when(LATITUDE=='N'~ +(NR_GRAU_LAT + (NR_MIN_LAT/60) + (NR_SEG_LAT/60^2)),
                     LATITUDE=='S'~ - (NR_GRAU_LAT + (NR_MIN_LAT/60) + (NR_SEG_LAT/60^2))
           ),
         long_dec =-(NR_GRAU_LONG+(NR_MIN_LONG/60)+(NR_SEG_LONG/60^2))) %>% 
  select(-NR_GRAU_LAT,-NR_MIN_LAT,-NR_SEG_LAT,-NR_GRAU_LONG,-NR_MIN_LONG,
         -NR_SEG_LONG)

#filtering all lat long that has NA
data_22_noCoord2 = data_22_Coord %>% 
  filter(is.na(lat_dec) | is.na(long_dec) ) %>% as_tibble

good_data_coord2 = data_22_Coord %>% 
  filter( !is.na(lat_dec) &
            !is.na(long_dec) ) %>% as_tibble

#Merging the data_22_noCoord2 with data_22_noCoord
data_22_noCoord3 = bind_rows(data_22_noCoord2 %>% select(-lat_dec,-long_dec),data_22_noCoord)

#checking number of rows
data_22_noCoord3 %>% nrow+good_data_coord2 %>% nrow ==data_22 %>% nrow#TRUE

#Saving those dataframes:
good_data_coord2 %>% 
  write_rds(
    file = 'E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\22_goodDataCoord.rds')
data_22_noCoord3 %>% 
  write_rds('E:\\MBA DATA SCIENCE\\TCC\\MiddleData\\22_badDataNoCoord.rds')
