library(corrplot)
library(tidyverse)
# Parte 1: Primeira analise do vif:
vif_test = read_rds('VifTeste01.rds')


corrplot(vif_test$correlation_variables, method = 'number',diag = F)
#Analisando correlacao
upper.tri(vif_test$correlation_variables)
upper<-vif_test$correlation_variables
upper[upper.tri(vif_test$correlation_variables)]<-""
upper<-as.data.frame(upper)
upper%>%write_csv2('CorTable.csv')
view(upper)

vifes = unlist(vif_test$vif_values)

options(scipen=999)
nome_var = c(vif_test$sample_values%>%names)
data_frame = data.frame(nomessss = nome_var, vasdasd= vifes)
data_frame%>%filter(vasdasd>10)

##########################################################
# Analisando o 2o vif
vif_test2 = read_rds('VifTeste02.rds')
upper.tri(vif_test2$correlation_variables)
upper<-vif_test2$correlation_variables
upper[upper.tri(vif_test2$correlation_variables)]<-""
upper<-as.data.frame(upper)
view(upper)
vif_test2$data_frame_vif_var


#Lendo o raster e retendo as var que queremos
library(terra)
stack_raster <- terra::rast('./data_clim/Variables/stack_raster.tif')
stack_new= c(stack_raster[[1]],stack_raster[[2]],stack_raster[[5]],stack_raster[[7]],stack_raster[[17]],stack_raster[[20]],stack_raster[[21]],stack_raster[[22]])
stack_new%>%writeRaster('./data_clim/Variables/Variables_present_ready.tif',overwrite=T)


