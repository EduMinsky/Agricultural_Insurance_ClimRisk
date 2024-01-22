###### Exploring the possibles issues with the Latitude and Longitude Data #####
lapply(c('tidyverse','sf'),library,character.only = TRUE)
data_06_21 = read_rds('./MiddleData/data_clean_06_21.rds')
data_22 = read_rds('./MiddleData/data_clean_22.rds')
#Working with the 06-21 data first:
data_06_21 %>% group_by(LATITUDE) %>% count()
# 1.130.099 lines with South information
# 40 lines with North information
#186.219 with empty info

data_06_21 %>% group_by(LONGITUDE) %>% count()
# 1.130.145 lines with West information
# 18 lines with East information
# 186.195 with empty info

#Only a small portion of brazil is located at N latitude, let's explore a little
#more this data
data_06_21 %>% 
  filter(LATITUDE=='N') %>% 
  select(SG_UF_PROPRIEDADE ) %>% unique
# It is a bit weird because there is MS and PR states that are below the equator
# line.
# Further exploring this
data_06_21 %>% 
  filter(LATITUDE=='N') %>% 
  filter(SG_UF_PROPRIEDADE== 'MS'|
           SG_UF_PROPRIEDADE=='PR') %>% view

# It seems that there is a confusion, where should be S instead of N. However,
# We cant confirm that, so we should interpret that as an error!!!

#What about East? Brazil only occur in the west portion of the greenwitch line
data_06_21 %>% 
  filter(LONGITUDE=='E') %>% view

# This is completely wrong. We also should treat that as wrong info!
#Working with the data_22 data:
data_22%>% group_by(LATITUDE) %>% count
# 35.143 with empty info
# 17 records with N info
# 89.940 records with S info

#Exploring those 17 records with N info
data_22 %>% 
  filter(LATITUDE=='N') %>% 
  select(SG_UF_PROPRIEDADE ) %>% unique
# Majority of roraima is in Lat North, so it seems ok

#Exploring Longitude Values
data_22%>% group_by(LONGITUDE) %>% count
#35.143 values with empty info
#89.957 with W info




