library(tidyverse)
library(sf)
library(spdep)
set.seed(123)
# Lendo o dataframe e criando k-means
df_treino <- read_rds('./FinalData/DF_Treino.rds')
df_treino <- na.omit(df_treino)
#Calculando o Global Moran Index - primeiro para o set todo
df_treino_spat <- st_as_sf(df_treino,coords= c('x','y') )
df_treino_spat <- df_treino_spat %>% st_set_crs(unique(df_treino_spat$CRS))

# Criando a lista com os buffers dos pontos:
W_list <- st_buffer(df_treino_spat, dist = 0.1)
#Converter a lista em binario
W_list <- poly2nb(W_list)
W_list <- nb2listw(W_list, style = "B", zero.policy = TRUE)
# Indice de Moran:
moran_test <- moran.test(df_treino_spat$ID,W_list)



df_treino_var_only <- df_treino%>%select(-c(ID,x,y,CRS,Target))

res.km <- kmeans(scale(df_treino_var_only), centers = 3)
cluster_filters <- res.km$cluster
df_treino$Clusters <- cluster_filters
# Vamos Separar os grupos
cluster_1_Data <- df_treino%>%dplyr::filter(Clusters ==1)
cluster_2_Data <- df_treino%>%dplyr::filter(Clusters ==2)
cluster_3_Data <- df_treino%>%dplyr::filter(Clusters ==3)

#Criando nosso algoritmo de cosine
cluster_1_Data%>%slice(1:3)%>%view
c(742834.2148,319073.4187)
dist(rbind(c(742834.2148,319073.4187) , c(1030728.5497,884032.2075)))
rbind(c(742834.2148,319073.4187) , c(1030728.5497,884032.2075))

# Vamos escolher manhattan ou cosseno?
vec1 = c(-0.7873,-107.9503,51.4156,27.8733,13.8984,457.3835,335.0605,152.6665)
vec2 = c(0.1137,-193.1271,60.1564,27.9348,43.3178,460.1475,198.0943,61.7119)
vec3 = c(-0.6547,-119.6286,54.5428,25.2831,13.7925,396.365,365.187,89.6861)
cosine(vec1,vec3)
cosine(vec1,vec2)
cosine(vec2,vec3)
dist(rbind(vec1,vec2),method = "manhattan")

scale(rbind(vec1,vec2))
