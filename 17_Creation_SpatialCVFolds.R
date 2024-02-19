library(tidyverse)
library(factoextra)
library(tidymodels)
library(spatialsample)
library(rsample)
library(sf)
set.seed(123)

data1 <- read_rds('./FinalData/Final_Data_readyToUse.rds')%>%as_tibble%>%select(-ID) %>% mutate(Target =factor(Target, levels = c("0","1")) )


# Muitas vezes a separação entre pontos para treino teste e validaçao pode levar a criacao de subsets onde os pontos são muito proximos um dos outros
# Vamos tratar isso atraves de um spatial CV. 

data1_sp <-st_as_sf(data1,coords=c("x","y"),crs = 'proj4: +proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs +type=crs')
cv_spat <- spatial_clustering_cv(data1_sp, v = 4)
autoplot(cv_spat)

