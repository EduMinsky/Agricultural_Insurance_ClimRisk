##################################################################################
# Script Para extrair os valores das variaveis para os pontos dos seguros rurais #
##################################################################################
library(sf)
library(terra)
library(raster)
library(tidyverse)
#Lendo as variaveis
variables <- terra::rast('./data_clim/Variables/Variables_present_ready.tif')

#Lendo os shapefiles:
data_0621_event_0 <- read_sf('MiddleData/not_occurring_06_21_shp_unique.shp')
data_0621_event_1 <- read_sf('MiddleData/occuring_06_21_shp_unique.shp')
data22_event_0 <- read_sf('MiddleData/not_occurring_data_22_shp_unique.shp')

# Projetando nossos dados para South America Albers Equal Area
# proj4: +proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs +type=crs
projecao <- '+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs +type=crs'
variables_SA_Albers <- terra::project(x = variables,projecao, method = 'cubic')
treino_event_0_SA_Albers <- st_transform(x = data_0621_event_0,
                                        crs =projecao )

treino_event_1_SA_Albers <- st_transform(x = data_0621_event_1,
                                        crs =projecao )

teste_event_0_SA_Albers <- st_transform(x = data22_event_0,
                                        crs =projecao )


#Extraindo os valores das variaveis para os pontos:
var_treino_event_0 <- terra::extract(x = variables_SA_Albers,
                                    y = treino_event_0_SA_Albers,
                                    method = "simple",
                                    xy=TRUE,
                                    ID=TRUE)


var_treino_event_1 <- terra::extract(x = variables_SA_Albers,
                                    y = treino_event_1_SA_Albers,
                                    method = "simple",
                                    xy=TRUE,
                                    ID=TRUE)

var_teste_event_0 <- terra::extract(x = variables_SA_Albers,
                                    y = teste_event_0_SA_Albers,
                                    method = "simple",
                                    xy=TRUE,
                                    ID=TRUE)

# Criando uma coluna com o nome da projecao para facilitar a transformação em shapefile no Futuro
# Pode ser util
var_treino_event_0$CRS = projecao
var_treino_event_1$CRS = projecao
var_teste_event_0$CRS = projecao

#Criando a target variable:
var_treino_event_0$Target = as.factor(0)
var_treino_event_1$Target = as.factor(1)
var_teste_event_0$Target = as.factor(0)

# Salvando como DataFrame essas tabelas:
bind_rows(var_treino_event_0,var_treino_event_1,var_teste_event_0)%>%write_rds('./FinalData/DF_VAR.rds')
getwd()

