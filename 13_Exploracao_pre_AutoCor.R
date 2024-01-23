###########################################################################################
# Script Para explorar a relação dos registros de Seguro Rural com as variaveis ambientais#
###########################################################################################
library(tidyverse)
library(conflicted)
library(tidymodels)
library(factoextra)
library(ggpubr)
library(viridis)
library(hrbrthemes)


# Vamos criar uma PCA para ver a interação dos dados em um "ambiente" reduzido em dimensionalidade
df_treino <- read_rds('./FinalData/DF_Treino.rds')
df_treino <- na.omit(df_treino)
df_treino <- df_treino%>%select(-c(ID,x,y,CRS))

# Fazendo um sample para conseguir plotar bem os dados
samples <- sample(1:nrow(df_treino),500,replace=F)
df_treino_samples <- df_treino[samples,]

res.pca <- prcomp(df_treino_samples[1:8], scale = TRUE)
fviz_eig(res.pca)
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )

groups <- as.factor(df_treino_samples$Target)
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
             )

# Nossos dados estão muito clusterizados. não da para separar entre os dados do grupo 1 e grupo 2
# Vamos aplicar um Kmean para vizualisar de uma maneira diferente
# Nós queremos, se possivel, identificar se, na hora de aplicar uma técnica para diminuir a auto correlação espacial, 
# poderemos fazer isso separadamente para cada grupo (grupo 1 ou 0) ou não.
# A intenção é reter o maximo possivel da originalidade dos dados mas se, ao percebemos, que os dois grupos são parecidos,
# Entao vamos diminuir essa auto-correlação unicamente.
df_treino%>%select(-Target)
res.km <- kmeans(scale(df_treino%>%select(-Target)), centers = 3)

# Dimension reduction using PCA
res.pca <- prcomp(df_treino%>%select(-Target),  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Target groups from the original data sett
ind.coord$Targets <- df_treino$Target
# Data inspection
head(ind.coord)
# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "Targets", size = 2.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)

df_treino$Clusters <- res.km$cluster
df_treino$Clusters <- as.factor(df_treino$Clusters)
# Criando novamente a PCA para fazer um plot dos dois pcs
res.pca <- prcomp(df_treino[1:8], scale = TRUE)
pcs <- res.pca$x%>%as_tibble
pcs_1_2 <- tibble(Axes_1 = pcs$PC1, Axes_2 = pcs$PC2)
pcs_1_2$Clusters <- res.km$cluster
pcs_1_2$Clusters <- as.factor(pcs_1_2$Clusters)
ggplot(pcs_1_2, aes(x=Axes_1, y=Axes_2, shape=Clusters, color=Clusters, size=Clusters)) +
  geom_point()


