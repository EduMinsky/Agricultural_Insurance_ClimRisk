library(tidymodels)
library(tidyverse)
library(spatialsample)
library(sf)
data <- read_rds('./FinalData/Final_Data_readyToUse.rds')%>%as_tibble%>%select(-ID) %>% mutate(Target =factor(Target, levels = c("0","1")) )
data <- data%>%st_as_sf(coords=c("x","y"),crs="+proj=aea +lat_0=-32 +lon_0=-60 +lat_1=-5 +lat_2=-42 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs +type=crs")
data_split <- initial_split(data, prop = 0.80, strata = Target)
data_training <- training(data_split)
data_testing <- testing(data_split)
data_fold <- spatial_clustering_cv(data_training, v = 5)

# Criando um modelo logistico
log_model <-logistic_reg() %>% set_engine("glm")%>% translate() # Criando um objeto do tipo regressao logistica

log_model_fit <- log_model %>% fit(Target ~. ,data = data_training%>%st_drop_geometry)# Aplicando esse objeto a nossos dados atraves da funcao fit

model_res <- 
  log_model_fit %>% 
  extract_fit_engine() %>% 
  summary() #Extraindo o summary do modelo
tidy(log_model_fit)#Transformando o summary que estava em uma matriz numa tibble
model_test_result <- predict(log_model_fit,new_data = data_testing) #Fazendo prediction para o grupo de teste
# Unindo esse resultado ao dado de teste
data_testing%>% select(Target) %>%bind_cols(model_test_result)
parsnip_addin()#Testando a GUI do parsnip


