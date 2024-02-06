library(tidyverse)
library(sf)
library(terra)
library(factoextra)
library(ggpubr)
library(spdep)
set.seed(123)
projecao <- '+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs'
# Lendo shapefiles
cluster_one_rare <- st_read('./SupportData/Shapefiles_data_train/Cluster1_WithinDistance.shp')%>%dplyr::select(geometry,Target,Clustrs)
cluster_two_rare <- st_read('./SupportData/Shapefiles_data_train/Cluster2_WithinDistance.shp')%>%dplyr::select(geometry,Target,Clustrs)
cluster_three_rare <- st_read('./SupportData/Shapefiles_data_train/Cluster3_WithinDistance.shp')%>%dplyr::select(geometry,Target,Clustrs)

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

var_cluster_one_rare$Target <- cluster_one_rare$Target
var_cluster_two_rare$Target <- cluster_two_rare$Target
var_cluster_three_rare$Target <- cluster_three_rare$Target
# Plotando os novos clusters

df_all <- rbind(var_cluster_one_rare,var_cluster_two_rare,var_cluster_three_rare)

res.km <- kmeans(scale(df_all%>%select(-Target,-ID,-x,-y)), centers = 3)
# Dimension reduction using PCA
res.pca <- prcomp(df_all%>%select(-Target,-ID,-x,-y),  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Target groups from the original data sett
ind.coord$Targets <- df_all$Target
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
res.pca <- prcomp(df_all%>%select(-Target,-ID,-x,-y), scale = TRUE)
pcs <- res.pca$x%>%as_tibble
pcs_1_2 <- tibble(Axes_1 = pcs$PC1, Axes_2 = pcs$PC2)
pcs_1_2$Clusters <- res.km$cluster
pcs_1_2$Clusters <- as.factor(pcs_1_2$Clusters)
ggplot(pcs_1_2, aes(x=Axes_1, y=Axes_2, shape=Clusters, color=Clusters, size=Clusters)) +
  geom_point()

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


df_all%>%write_rds('./FinalData/Final_Train_Data_readyToUse.rds')
