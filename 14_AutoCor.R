library(tidyverse)
library(sf)
library(spdep)
library(purrr)
set.seed(123)

# Lendo o dataframe
df_treino <- read_rds('./FinalData/DF_Treino.rds')
df_treino <- na.omit(df_treino)
df_treino%>%names

kmean_groups <- function(df_data,columns_to_leave ){
    # Preparando o DF para o kmean
    df_data_filter <- df_data%>%select(-c(columns_to_leave))
    # Criando o kmean:
    kmean_result <- kmeans(scale(df_data_filter), centers = 3)
    clusters_filters <- kmean_result$cluster
    df_data$Clusters <- clusters_filters
    # Separando os Clusters
    clusters_1_Data <- df_data%>%dplyr::filter(Clusters ==1)
    clusters_2_Data <- df_data%>%dplyr::filter(Clusters ==2)
    clusters_3_Data <- df_data%>%dplyr::filter(Clusters ==3)
    return(list(Cluster_1 = clusters_1_Data, Cluster_2 = clusters_2_Data, Cluster_3 = clusters_3_Data))    
}
list_groupsdata <- kmean_groups(df_data = df_treino,columns_to_leave = c("x","y","CRS","Target"))

#Calculando o Global Moran Index
# Criando a funcao para isso
spatialize_and_moranI <- function(original_data){
    df_treino_spat <- st_as_sf(original_data,coords= c('x','y') )
    df_treino_spat <- df_treino_spat %>% st_set_crs(unique(df_treino_spat$CRS))
    # Criando a lista com os buffers dos pontos:
    W_list <- st_buffer(df_treino_spat, dist = 0.1)
    #Converter a lista em binario
    W_list <- poly2nb(W_list)
    W_list <- nb2listw(W_list, style = "B", zero.policy = TRUE)
    # Indice de Moran:
    moran_test <- moran.test(df_treino_spat$ID,W_list)
    return(moran_test)
}
# Para o set todo
moran_test_total <- spatialize_and_moranI(df_treino)
# Para os clusters:
moran_test_clu1 <- spatialize_and_moranI(list_groupsdata[[1]])
moran_test_clu2 <- spatialize_and_moranI(list_groupsdata[[2]])
moran_test_clu3 <- spatialize_and_moranI(list_groupsdata[[3]])

# Criando funcao para preparar os dados para o ANN Index
chull_shape_area <- function(data_frame,x_col,y_col,CRS){
    shapefile_ <- st_as_sf(data_frame,coords = c(x_col,y_col))%>%st_set_crs(CRS)
    convex_hull<-st_convex_hull(st_union(shapefile_)) 
    area_chull <- st_area(convex_hull)%>%as.numeric
    return(list(convex_hull_shape = convex_hull, chull_area = area_chull))
}
cluster_1_outputs <- chull_shape_area(list_groupsdata[[1]], "x","y",list_groupsdata[[1]]$CRS%>%unique)
cluster_2_outputs <- chull_shape_area(list_groupsdata[[2]], "x","y",list_groupsdata[[2]]$CRS%>%unique)
cluster_3_outputs <- chull_shape_area(list_groupsdata[[3]], "x","y",list_groupsdata[[3]]$CRS%>%unique)

# Salvando alguns dados para rodar no arcgis
cluster_1_outputs$convex_hull_shape%>%st_write('chull_cluster1.shp')
cluster_2_outputs$convex_hull_shape%>%st_write('chull_cluster2.shp')
cluster_3_outputs$convex_hull_shape%>%st_write('chull_cluster3.shp')

list_groupsdata[[1]]%>%st_as_sf(coords= c('x','y') )%>% st_set_crs(unique(list_groupsdata[[1]]$CRS))%>%st_write('Cluster1.shp',append=FALSE)
list_groupsdata[[2]]%>%st_as_sf(coords= c('x','y') )%>% st_set_crs(unique(list_groupsdata[[2]]$CRS))%>%st_write('Cluster2.shp')
list_groupsdata[[3]]%>%st_as_sf(coords= c('x','y') )%>% st_set_crs(unique(list_groupsdata[[3]]$CRS))%>%st_write('Cluster3.shp')
df_treino%>%st_as_sf(coords= c('x','y') )%>% st_set_crs(unique(df_treino$CRS))%>%st_write('df_treino.shp')

