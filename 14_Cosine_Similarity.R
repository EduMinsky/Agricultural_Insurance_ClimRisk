library(tidyverse)
library(conflicted)
library(tidymodels)
library(factoextra)
# Lendo o dataframe e criando k-means
df_treino <- read_rds('./FinalData/DF_Treino.rds')
df_treino <- na.omit(df_treino)
df_treino <- df_treino%>%select(-c(ID,x,y,CRS))

df_treino%>%select(-Target)
res.km <- kmeans(scale(df_treino%>%select(-c(Target,ID,x,y,CRS))), centers = 3)
cluster_filters <- res.km$cluster
df_treino$Clusters <- cluster_filters
# Vamos comparar os grupos
cluster_1_Data <- df_treino%>%dplyr::filter(Clusters ==1)
cluster_2_Data <- df_treino%>%dplyr::filter(Clusters ==2)
cluster_3_Data <- df_treino%>%dplyr::filter(Clusters ==3)

cluster_1_Data$anu_mean_temp_anomaly%>%mean
cluster_2_Data$anu_mean_temp_anomaly%>%mean
cluster_3_Data$anu_mean_temp_anomaly%>%mean