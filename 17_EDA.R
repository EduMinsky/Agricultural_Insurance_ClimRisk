library(tidyverse)
library(car)
library(terra)
library(factoextra)
library(gridExtra)
library(sf)
set.seed(123)

data <- read_rds('./FinalData/Final_Data_readyToUse.rds')%>%as_tibble%>%select(-ID) %>% mutate(Target =factor(Target, levels = c("0","1")) )

# Qual a proporção de observações que tiveram o seguro acionado (fator1) e dos que não tiveram:
data%>%filter(Target==1)%>%nrow / data%>%nrow
data%>%filter(Target==0)%>%nrow / data%>%nrow
# Cerca de 33% dos dados acionaram o seguro e o restante não

# Como esses dados estão dispostos no espaço ambiental?
# Vamos plotar uma PCA para isso

# Primeiro vamos criar tambem pontos de background
variables <- terra::rast('./data_clim/Variables/Variables_present_ready.tif')
variables <- terra::project(x = variables,"+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs +type=crs", method = 'cubic')
#Fazendo sample de pontos aleatorios
random_samples <- terra::spatSample(variables, size = 10000, method = 'random', na.rm = TRUE, as.df = TRUE,xy=TRUE)
random_samples <- random_samples%>%as_tibble
random_samples$Target = "BG"

# Unindo os dados para plotar a PCA
data_PCA <- rbind(data,random_samples)
pca_result <- prcomp(data_PCA%>%select(-Target,-x,-y),  
                   scale = TRUE)
PC1<-pca_result$x[,1]
PC2<-pca_result$x[,2]


plot_pca <- ggplot(data_PCA, 
       aes(x = PC1, 
           y = PC2, 
           color = Target))+
  #geom_point() 
  stat_ellipse()+ scale_color_manual(values = c("red", "blue", "black"))


#Plotando Kmeans
res.km <- kmeans(scale(data_PCA%>%select(x,y)), centers = 4)
data_PCA$cluster <- factor(res.km$cluster)

plot_kmean <- ggplot(data_PCA, aes(x=x, y=y,color=Target)) +
  geom_point()+ scale_color_manual(values = c("red", "blue", "black"))
grid.arrange(plot_pca, plot_kmean,heights=c(2,2)) 

# Podemos perceber, pela Elipse, que grande parte do espaço ambiental dos seguros acionados (Target ==1)
# E dos seguros não acionados (Target==0) possuem alguma similaridade e que alguma proporção dos nossos dados (Target 1 e 0) representam grande parte do background
# Portanto, nosso modelo, ao projetar os dados no espaço geografico pode não ser tao truncado.

# Vamos plotar os histogramas de cada variavel para ver a distribuicao
# Plotando para as variaveis de temperatura
# Para bio3 e 5
bio_3_plot <- ggplot(data_PCA, aes(bio3)) + 
   geom_histogram(data =data_PCA%>%filter(Target==0) ,bins = 300, fill = "red", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target==1) ,bins = 300, fill = "blue", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target=="BG") ,bins = 300, fill = "black", alpha = 0.5)
bio_5_plot <- ggplot(data_PCA, aes(bio5)) + 
   geom_histogram(data =data_PCA%>%filter(Target==0) ,bins = 300, fill = "red", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target==1) ,bins = 300, fill = "blue", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target=="BG") ,bins = 300, fill = "black", alpha = 0.5)
grid.arrange(bio_3_plot, bio_5_plot,heights=c(2,2)) 

# Para variaveis de precipitacao
# Bio 15 18 19

bio_15_plot <- ggplot(data_PCA, aes(bio15)) + 
   geom_histogram(data =data_PCA%>%filter(Target==0) ,bins = 300, fill = "red", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target==1) ,bins = 300, fill = "blue", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target=="BG") ,bins = 300, fill = "black", alpha = 0.5)
bio_18_plot <- ggplot(data_PCA, aes(bio18)) + 
   geom_histogram(data =data_PCA%>%filter(Target==0) ,bins = 300, fill = "red", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target==1) ,bins = 300, fill = "blue", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target=="BG") ,bins = 300, fill = "black", alpha = 0.5)
bio_19_plot <- ggplot(data_PCA, aes(bio19)) + 
   geom_histogram(data =data_PCA%>%filter(Target==0) ,bins = 300, fill = "red", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target==1) ,bins = 300, fill = "blue", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target=="BG") ,bins = 300, fill = "black", alpha = 0.5)
grid.arrange(bio_15_plot, bio_18_plot,bio_19_plot,heights=c(2,2)) 

# Para as anomalias e terreno
anom_prec<-ggplot(data_PCA, aes(anu_precipitation_anomaly)) + 
   geom_histogram(data =data_PCA%>%filter(Target==0) ,bins = 300, fill = "red", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target==1) ,bins = 300, fill = "blue", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target=="BG") ,bins = 300, fill = "black", alpha = 0.5)

data_PCA <- data_PCA%>%rename(Terrain_Rugg = `terrain ruggedness index`)
terrain_rug<-ggplot(data_PCA, aes(Terrain_Rugg)) + 
   geom_histogram(data =data_PCA%>%filter(Target==0) ,bins = 300, fill = "red", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target==1) ,bins = 300, fill = "blue", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target=="BG") ,bins = 300, fill = "black", alpha = 0.5)

anom_temp<-ggplot(data_PCA, aes(anu_mean_temp_anomaly)) + 
   geom_histogram(data =data_PCA%>%filter(Target==0) ,bins = 300, fill = "red", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target==1) ,bins = 300, fill = "blue", alpha = 0.8)+
   geom_histogram(data =data_PCA%>%filter(Target=="BG") ,bins = 300, fill = "black", alpha = 0.5)

grid.arrange(anom_prec, terrain_rug,anom_temp,heights=c(2,2)) 


