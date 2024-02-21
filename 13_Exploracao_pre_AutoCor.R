library(tidyverse)
library(conflicted)
library(tidymodels)
library(factoextra)
library(ggpubr)
library(viridis)
library(hrbrthemes)
# Vamos criar uma PCA para ver a interação dos dados em um "ambiente" reduzido em dimensionalidade
df_treino <- read_rds('./FinalData/DF_VAR_TRAIN.rds')
df_treino <- na.omit(df_treino)
df_treino <- df_treino%>%select(-c(ID,CRS))
df_treino$DataType = "Train"

df_test <- read_rds('./FinalData/DF_VAR_VALIDATION.rds')
df_test <- na.omit(df_test)
df_test <- df_test%>%select(-c(ID,CRS))
df_test$DataType = "Test"

df_all <- bind_rows(df_treino,df_test)
# Realizando o kmeans entre os pontos geograficos
res.km <- kmeans(scale(df_all%>%select(x,y)), centers = 4)
df_all$cluster <- factor(res.km$cluster)

#Plotando esses clusters

ggplot(df_all, aes(x=x, y=y, color=cluster)) +
  geom_point()
# Conseguimos separar em 4 clusters geograficos nossos dados
# 4 Clusters parece um numero bom para fazer futuramente o spatial cluster

# Realizando uma PCA para ver se faz sentido separar o ano de 2023 em Teste independente
pca_result <- prcomp(df_all%>%select(-Target,-x,-y,-cluster,-DataType),  
                   scale = TRUE)
PC1<-pca_result$x[,1]
PC2<-pca_result$x[,2]
plot_pca <- ggplot(df_all, 
       aes(x = PC1, 
           y = PC2, 
           color = DataType))+
  geom_point() +
  stat_ellipse()+ scale_color_manual(values = c("red", "blue"))
plot_pca
