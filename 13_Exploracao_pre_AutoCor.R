library(tidyverse)
library(conflicted)
library(tidymodels)
library(factoextra)
library(ggpubr)
library(viridis)
library(hrbrthemes)
# Vamos criar uma PCA para ver a interação dos dados em um "ambiente" reduzido em dimensionalidade
df_treino <- read_rds('./FinalData/DF_VAR.rds')
df_treino <- na.omit(df_treino)
df_treino <- df_treino%>%select(-c(ID,CRS))

# Realizando o kmeans entre os pontos geograficos
res.km <- kmeans(scale(df_treino%>%select(x,y)), centers = 4)
df_treino$cluster <- factor(res.km$cluster)

#Plotando esses clusters

ggplot(df_treino, aes(x=x, y=y, color=cluster)) +
  geom_point()
# Conseguimos separar em 4 clusters geograficos nossos dados
# Vamos entao trabalhar nisso
