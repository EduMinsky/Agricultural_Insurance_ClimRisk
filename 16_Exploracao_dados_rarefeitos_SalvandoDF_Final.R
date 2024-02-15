library(tidyverse)
library(sf)
library(terra)
library(factoextra)
library(ggpubr)
library(spdep)
set.seed(123)
projecao <- '+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs'
# Lendo shapefiles
cluster_one_rare <- st_read('./SupportData/Shapefiles_data_train/Cluster1_ReadytoUse.shp')%>%dplyr::select(geometry,Target,Clustrs)
cluster_two_rare <- st_read('./SupportData/Shapefiles_data_train/Cluster2_ReadytoUse.shp')%>%dplyr::select(geometry,Target,Clustrs)
cluster_three_rare <- st_read('./SupportData/Shapefiles_data_train/Cluster3_ReadytoUse.shp')%>%dplyr::select(geometry,Target,Clustrs)
cluster_four_rare <- st_read('./SupportData/Shapefiles_data_train/Cluster4_ReadytoUse.shp')%>%dplyr::select(geometry,Target,Clustrs)

# Extraindo novamente os valores das VAR
variables <- terra::rast('./data_clim/Variables/Variables_present_ready.tif')
variables_SA_Albers <- terra::project(x = variables,projecao, method = 'cubic')

var_cluster_one_rare <- terra::extract(x = variables_SA_Albers,
                                    y = cluster_one_rare,
                                    method = "simple",
                                    xy=TRUE,
                                    ID=TRUE)

var_cluster_two_rare <- terra::extract(x = variables_SA_Albers,
                                    y = cluster_two_rare,
                                    method = "simple",
                                    xy=TRUE,
                                    ID=TRUE)

var_cluster_three_rare <- terra::extract(x = variables_SA_Albers,
                                    y = cluster_three_rare,
                                    method = "simple",
                                    xy=TRUE,
                                    ID=TRUE)

var_cluster_four_rare <- terra::extract(x = variables_SA_Albers,
                                    y = cluster_four_rare,
                                    method = "simple",
                                    xy=TRUE,
                                    ID=TRUE)

var_cluster_one_rare$Target <- cluster_one_rare$Target
var_cluster_two_rare$Target <- cluster_two_rare$Target
var_cluster_three_rare$Target <- cluster_three_rare$Target
var_cluster_four_rare$Target <- cluster_four_rare$Target
df_all <- rbind(var_cluster_one_rare,var_cluster_two_rare,var_cluster_three_rare,var_cluster_four_rare)

# Recalculando o Moran
df_all%>%tibble
df_treino_spat <- st_as_sf(df_all,coords= c('x','y') )
df_treino_spat <- df_treino_spat %>% st_set_crs('+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs +type=crs')
    # Criando a lista com os buffers dos pontos:
    W_list <- st_buffer(df_treino_spat, dist = 0.1)
    #Converter a lista em binario
    W_list <- poly2nb(W_list)
    W_list <- nb2listw(W_list, style = "B", zero.policy = TRUE)
    # Indice de Moran:
    moran_test <- moran.test(df_treino_spat$ID,W_list)


df_all%>%write_rds('./FinalData/Final_Data_readyToUse.rds')
