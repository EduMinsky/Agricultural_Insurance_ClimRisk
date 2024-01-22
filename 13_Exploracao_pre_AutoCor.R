###########################################################################################
# Script Para explorar a relação dos registros de Seguro Rural com as variaveis ambientais#
###########################################################################################
library(tidyverse)
library(conflicted)

df_treino <- read_rds('./FinalData/DF_Treino.rds')

df_treino%>%view
