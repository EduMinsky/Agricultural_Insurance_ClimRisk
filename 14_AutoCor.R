library(tidyverse)
library(sf)
library(spdep)
set.seed(123)
# Lendo o dataframe
df_treino <- read_rds('./FinalData/DF_Treino.rds')
df_treino <- na.omit(df_treino)
# Calculando os clusters
df_treino_var_only <- df_treino%>%select(-c(ID,x,y,CRS,Target))

res.km <- kmeans(scale(df_treino_var_only), centers = 3)
cluster_filters <- res.km$cluster
df_treino$Clusters <- cluster_filters
# Vamos Separar os grupos
cluster_1_Data <- df_treino%>%dplyr::filter(Clusters ==1)
cluster_2_Data <- df_treino%>%dplyr::filter(Clusters ==2)
cluster_3_Data <- df_treino%>%dplyr::filter(Clusters ==3)
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
moran_test_clu1 <- spatialize_and_moranI(cluster_1_Data)
moran_test_clu2 <- spatialize_and_moranI(cluster_2_Data)
moran_test_clu3 <- spatialize_and_moranI(cluster_3_Data)

# Todos os dados estao com um indice de Moran alto, muito acima do experado
# Logo, precisamos mesmo tratar esse problema.
# Vamos atacar atraves de um teste de distancia media do vizinho mais proxima
# Source: https://pro.arcgis.com/en/pro-app/3.1/tool-reference/spatial-statistics/h-how-average-nearest-neighbor-distance-spatial-st.htm

# Criando nosso algoritmo para isso:


lista_distancia  <-list()
lista_contraquem <- list()
for(i in 1:nrow(cluster_1_Data)){
    distancia <- dist(rbind(cluster_1_Data[1,c("x","y")], cluster_1_Data[i+1,c("x","y")]))
    lista_distancia[[i]] <- distancia
    lista_contraquem [[i]] <- paste0(1,'_contra_',i+1)
}
teste <- data.frame(Val_Dist = unlist(lista_distancia), Quem_contra_quem = unlist(lista_contraquem))
teste%>%view
