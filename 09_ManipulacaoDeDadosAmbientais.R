library(sf)
library(terra)
library(raster)
library(tidyverse)
library(purrr)
library(httr)
library(jsonlite)
library(geojsonsf)

############################################################
# Objetivo deste script:                                   #
    # Cortar os rasters do bioclim na área de interesse    #
    # Calcular os rasters de anomalia                      #
############################################################

# Criar função para cortar os rasters do presente para o Brasil e depois para o Minimun convex polygon (convex hull) dos registros totais que
#usaremos na modelagem

read_mask_crop_raster <- function(path_raster, path_points,names_points) {
    
    suppressWarnings({#Ler os rasters
    print('Lendo Rasters')
   rast_file <- list(list.files(path_raster, full.names=T))
   rast <- pmap(.l=rast_file,.f=terra::rast)
   #Baixand o mapa do brasil
   print('Baixando mapa do brasil')
   brazil =geojson_sf("https://servicodados.ibge.gov.br/api/v3/malhas/paises/BR?formato=application/vnd.geo+json&qualidade=maxima")
   #Cortar os rasters climaticos pelo brazil
   print('Maskarando os rasters pelo BRazil')
   rast_cortados <- map(rast, .f= ~raster::mask(x = ., mask = brazil),.progress=TRUE)
   print('Croppando os rasters pelo BRazil')
   rast_cropados <- map(rast_cortados, .f = ~raster::crop(x=., y = brazil),.progress=TRUE)
   })
   ###### Ler ShapeFiles ######
   print('Lendo os Shapefiles')
   file_names <- str_c(path_points,names_points)
   list_shp <- map(.x = file_names,.f= st_read)
   print('Unindo os Shapefiles')
   list_shape_union <-  map_vec(.x= list_shp, .f= st_union)
   list_shape_union <- list_shape_union%>%st_transform(4326)
   print('Criando o convex hull')
   convex_hull <- st_convex_hull(st_union(list_shape_union))
   #Buffando o chull
   chull_buffed = st_buffer(convex_hull%>%st_transform(5880), dist = 0.69)
   chull_buffed <- chull_buffed%>%st_transform(rast[[1]][[1]]%>%crs)
   ###### Mascarando e cortando os rasters pelo convexhull
   print('Maskarando os rasters pelo chull')
   rast_mask_chull <- map(rast_cortados, .f= ~raster::mask(x = ., mask = st_sf(chull_buffed)),.progress=TRUE)
   print('Croppando os rasters pelo chull')
   rast_cropados_chull <- map(rast_mask_chull, .f = ~raster::crop(x=., y = st_sf(chull_buffed)),.progress=TRUE)
   writeRaster(rast_cropados_chull[[1]], "./data_clim/wc2.1_country/bioc_chull.tif")
    return(rast_cropados_chull)
   }
#Aplicando nos rasters do bioclim
read_mask_crop_raster(path_raster = './data_clim/wc2.1_country/',
                    path_points = './MiddleData/',names_points =c('not_occurring_06_21_shp_unique.shp',
                                    'not_occurring_data_22_shp_unique.shp','occuring_06_21_shp_unique.shp') )


###########################################
# Criando  os dados de anomalias entre Presente - Passado - Usaremos Algebra de Mapas
#bio1 : Media anual de temperatura:
#bio12: Media anual de chuva
bio_1_passado = terra::rast("./data_clim/PaleoCLim//LH_v1_2_5m/bio_1.tif")
bio_12_passado =  terra::rast("./data_clim/PaleoCLim//LH_v1_2_5m/bio_12.tif")
#Lendo as variaveis do presente:
presente <- map('./data_clim/Variables/bioc_chull.tif',.f = terra::rast)

#Pegando as var 1 e 12 do presente:
bio_1_presente <- presente[[1]][[1]]
bio_12_presente <- presente[[1]][[12]]
# Fazendo o resample para que os rasters sejam na mesma resolucao
bio_1_passado_resample <- resample((bio_1_passado/10), bio_1_presente)
bio_12_passado_resample <- resample(bio_12_passado, bio_12_presente)
#Criando os dados de anomalias:
anu_mean_temp_anomaly = bio_1_presente - bio_1_passado_resample
anu_precipitation_anomaly = bio_12_presente - bio_12_passado_resample
#Salvando os dados:
anu_mean_temp_anomaly%>%writeRaster('./data_clim/Variables/anu_mean_temp_anomaly.tif')
anu_precipitation_anomaly%>%writeRaster('./data_clim/Variables/anu_precipitation_anomaly.tif')

# Calculando a Rugosidade do Terreno
# referencia: A Terrain Ruggedness Index that Quantifies Topographic Heterogeneity (Shawn J. Riley)
# referencia 2: https://gis.stackexchange.com/questions/6056/calculating-topographic-ruggedness-index-in-arcgis-desktop
#lendo o dem
DEM <- terra::rast('./data_clim/wc2.1_30s_elev/wc2.1_30s_elev.tif')
plot(DEM)
# Cortando e maskarando  o DEM para ter a mesma posicao
DEM_resample <- resample(DEM,bio_1_presente)
DEM_mask <- mask(DEM_resample,bio_1_presente)
DEM_crop <- crop(DEM_mask,bio_1_presente)
#Criando TRI
#computando focal sum
DEM_focal <- focal(DEM_crop, w=3, fun="sum")
#elevando o dem original ao quadrado
DEM_squared <- DEM_crop * DEM_crop
#focal sum do dem ao quadrado
DEM_squared_focal <- focal(DEM_squared, w=3, fun="sum")
#Computar o Indice ao quadrado
tri_squared <- (DEM_squared_focal) + 9*(DEM_squared) - 2*(DEM_crop)*(DEM_focal)
#Indice:
tri <- sqrt(tri_squared)
#Salvando:
writeRaster(tri, './data_clim/Variables/tri.tif')

