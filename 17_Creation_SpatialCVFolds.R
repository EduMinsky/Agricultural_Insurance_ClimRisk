library(tidyverse)
library(factoextra)
set.seed(123)
data <- read_rds('./FinalData/Final_Data_readyToUse.rds')%>%as_tibble%>%select(-ID,-x,-y) %>% mutate(Target =factor(Target, levels = c("0","1")) )

# Muitas vezes a separação entre pontos para treino teste e validaçao pode levar a criacao de subsets onde os pontos são muito proximos um dos outros
# Vamos tratar isso atraves de um spatial CV. A diferença é que vamos criar um cv para cada K, de um kmeans

res.km <- kmeans(scale(data%>%select(-Target)), centers = 3)
data$Clusters <- as.factor(res.km$cluster)
ggplot(data, aes(x=bio18, y=bio15, shape=Clusters, color=Clusters, size=Clusters)) +
  geom_point()
