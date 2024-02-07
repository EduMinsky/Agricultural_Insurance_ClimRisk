library(sf)
library(terra)
library(raster)
library(tidyverse)
library(purrr)
library(furrr)
library(httr)
library(jsonlite)
library(geojsonsf)
library(foreach)
library(doParallel)
library(future)
library(tictoc)

#############################################################
# Realizando VIF
# Importando o stack raster das variaveis
stack_raster <- terra::rast('./data_clim/Variables/stack_raster.tif')

vif_spatial <- function(n_sample, rast_files){
    #Iniciando o sample das variaveis
    if(nrow(rast_files)*ncol(rast_files) < n_sample){
        stop("Numero de amostras é maior que a quantidade de células do raster")
        
    }else if (n_sample < nlyr(rast_files)) {
       stop("Numero de amostras é menor que a quantidade de variaveis. R² explodirá ao infinito")
    }else{

        print('Iniciando sample')

        num_cores <- parallel::detectCores() - 2
        plan(multicore, workers = num_cores)
        data_frame_var_list <- future_map_dfr(1:num_cores, { ~terra::spatSample(rast_files, size = n_sample / num_cores, method = 'random', na.rm = TRUE, as.df = TRUE)},.options=furrr_options(seed = TRUE))     
        plan(NULL)
    }
    #Criando matriz de correlação para auxiliar na decisao:
    cor_variables = cor(data_frame_var_list)    
    
     print('Iniciando VIF')
     vif_value <- furrr::future_map(1:ncol(data_frame_var_list), ~ {
  i <- .x
  1 / (1 - summary(lm(data_frame_var_list[, i] ~ ., data = data_frame_var_list[, -i]))$adj.r.squared)
},.options=furrr_options(seed = TRUE))  
     vif_value <- as.list(vif_value)

     data_frame_vif_variables = data.frame(vif_values = unlist(vif_value), var_name = c(data_frame_var_list%>%names()))
    print('Feito!')
    
    return (list(sample_values = data_frame_var_list,vif_values = vif_value, correlation_variables = cor_variables, data_frame_vif_var = data_frame_vif_variables))
}


tic()
vif_test = vif_spatial(n_sample = 1000000,rast_files =stack_raster )
toc()
vif_test$data_frame_vif_var
#Salvando
vif_test %>% write_rds('VifTeste01.rds')




#Segunda rodada do VIF:
stack_raster <- terra::rast('./data_clim/Variables/stack_raster.tif')
stack_new= c(stack_raster[[1]],stack_raster[[2]],stack_raster[[5]],stack_raster[[7]],stack_raster[[17]],stack_raster[[20]],stack_raster[[21]],stack_raster[[22]])
stack_new%>%names
tic()
vif_test2 = vif_spatial(n_sample = 1000000,rast_files =stack_new )
toc()
vif_test2 %>% write_rds('VifTeste02.rds')
