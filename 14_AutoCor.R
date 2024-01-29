library(tidyverse)
library(sf)
library(spdep)
library(purrr)
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
test_df <- cluster_1_Data[1:100,]
lista_valores <- vector("list")
for(i in 1:(nrow(test_df)-1)){
    lista_valores[[i]] <- vector("list")
    for(j in (i+1): nrow(test_df)){
        lista_valores[[i]][[j]] <- dist(rbind(test_df[i,c("x","y")], test_df[j,c("x","y")]), upper = F, diag = F)
    }
}

valores_unlist <- map(lista_valores,.f = unlist)
data_frame_distance <- NULL
for(i in 1: length(valores_unlist)){
    data_frame_distance[[i]] <- tibble(Value_Distance = valores_unlist[[i]], Posicao_comparado = paste0("Comparacao da linha ", i, " contra o resto"))
}
data_frame_distance_all <- do.call(rbind,data_frame_distance)

test_df_sf <- st_as_sf(test_df,coords = c("x","y"))
test_df_sf <- test_df_sf%>%st_set_crs(test_df_sf$CRS%>%unique)
chull_test <-st_convex_hull(st_union(test_df_sf)) 
st_area(chull_test)%>%as.numeric
distancia_experada <- 0.5/ (sqrt(nrow(test_df)/ st_area(chull_test)%>%as.numeric))


data_frame_distance_all$Value_Distance%>%mean

ratio_distance <- data_frame_distance_all$Value_Distance%>%mean / distancia_experada


nearest_neighbor_distance_spatial <- function(data_, coord_x, coord_y, CRS){
    lista_valores <- vector("list")
    print("Iniciando o calculo da distancia para cada par")
    for(i in 1 : (nrow(data_)-1)){
        lista_valores[[i]] <- vector("list")
        for(j in (i+1):nrow(data_)){
            print(i)
            print(j)
            lista_valores[[i]][[j]] <- dist(rbind(data_[i,c(coord_x,coord_y)], data_[j,c(coord_x,coord_y)]), upper=F,diag=F)

        }    
    }
    print("Iniciando a criacao do dataframe de valores de distancia")
    valores_unlist <- map(lista_valores,.f = unlist)
    data_frame_distance <- NULL
    for(i in 1:length(valores_unlist)){
        data_frame_distance[[i]] <- tibble(value_Distance = valores_unlist[[i]], Posicao_comparado = paste0("Comparacao da linha ", i , " contra o resto"))
    }
    data_frame_distance_all <- do.call(rbind,data_frame_distance)
    print("Iniciando a criação do Convex Hull para calcular distancia experada")
    data_sf <- st_as_sf(data_,coords = c(coord_x,coord_y))
    data_sf <- data_sf%>%st_set_crs(CRS)
    chull_data <-st_convex_hull(st_union(data_sf))
    chull_area <- st_area(chull_data)%>%as.numeric
    print("Calculando distancia media e outras metricas")
    observed_mean_distance <- mean(data_frame_distance_all$value_Distance)
    expected_distance <- 0.5/ (sqrt(nrow(data_)/ chull_area))
    ratio_distance <- observed_mean_distance / expected_distance
    z_score <- (observed_mean_distance - expected_distance) / (0.26136/sqrt(nrow(data_)/chull_area))

    return(list(Distance_Data_Frame = data_frame_distance_all,observed_mean_distances = observed_mean_distance, expected_distances = expected_distance,
     Ratio_distance = ratio_distance, z_score_test = z_score, chull_area_data =chull_data ))
}

teste <-nearest_neighbor_distance_spatial(data_ = cluster_1_Data, coord_x = "x", coord_y = "y", CRS = cluster_1_Data$CRS%>%unique)

teste2 <-nearest_neighbor_distance_spatial(data_ = cluster_2_Data, coord_x = "x", coord_y = "y", CRS = cluster_2_Data$CRS%>%unique)

a = dist(rbind(cluster_1_Data[,c("x","y")], cluster_1_Data[,c("x","y")]), upper=F,diag=F)
a
mean(a)

teste$observed_mean_distances





dados <- data.frame(x = c(1, 2, 3, 4, 5),
                    y = c(2, 4, 1, 3, 5))

# Calcular a distância Euclidiana entre as colunas x e y
distancia <- dist(cluster_1_Data[, c("x", "y")])

# Converter a matriz de distâncias para um dataframe
distancia_dataframe <- as.data.frame(as.matrix(distancia))

# Ajustar os valores na diagonal inferior para NA
distancia_dataframe[lower.tri(distancia_dataframe)] <- NA

# Mostrar o dataframe de distâncias com a diagonal inferior eliminada
a = distancia_dataframe%>%tibble
a%>%tail
