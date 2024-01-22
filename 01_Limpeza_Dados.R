##### Import packages and data #####
lapply(c('tidyverse','sf'),library,character.only = TRUE)

getwd()
files = list.files('./DataTables/',full.names = T)
df_list = map(files, read.csv,sep = ';',header = T,fileEncoding='latin1')
data_06_15 = df_list[[1]]
data_16_21 = df_list[[2]]
data_22 = df_list[[3]]
##### Basic cleaning of data #####
#check for duplicity:
data_06_15$ID_PROPOSTA%>%duplicated()%>%sum()
data_16_21$ID_PROPOSTA%>%duplicated()%>%sum()
data_22$ID_PROPOSTA%>%duplicated()%>%sum()
#We have zero duplicated ids, so we are good to go
w
#All date columns are chr. Lets change that
#creating function to do that!

change_date_column = function(my.dataframe){
  my.dataframe = my.dataframe%>%mutate(Data_Apolice =dmy(my.dataframe$DT_APOLICE) 
                                       ,
                                       Data_Proposta =lubridate::dmy(my.dataframe$DT_PROPOSTA) 
                                       ,
                                       Data_Inicio_Vigencia =lubridate::dmy(my.dataframe$DT_INICIO_VIGENCIA) 
                                       ,
                                       Data_Fim_Vigencia =lubridate::dmy(my.dataframe$DT_FIM_VIGENCIA)  
                                       
  )%>%
    select(-c(DT_APOLICE,DT_PROPOSTA,DT_INICIO_VIGENCIA,DT_FIM_VIGENCIA))
  return(my.dataframe)
}
#Some basic manipulatino before applying this fun
data_16_21$DT_APOLICE = data_16_21$DT_APOLICE %>% 
  str_remove(pattern =' 00:00' )
data_22$DT_APOLICE = data_22$DT_APOLICE %>% 
  str_remove(pattern =' 00:00' )

#Applying function
data_06_15 = change_date_column(my.dataframe = data_06_15)
data_16_21 = change_date_column(my.dataframe = data_16_21)
data_22 = change_date_column(my.dataframe = data_22)

#Change the separator for decimal value in our dataframes

change.numerical.values = function(my.df){
  if(is.null(my.df$VALOR_INDENIZAÇÃO)==TRUE){
    my.df = my.df%>%mutate(Nmro_Proposta = as.numeric(NR_PROPOSTA),
                           Nmro_Prod_Estimada = as.numeric(NR_PRODUTIVIDADE_ESTIMADA),
                           Nmro_Prod_Segurada = as.numeric(NR_PRODUTIVIDADE_SEGURADA),
                           NivelCobertura = as.numeric(str_replace(my.df$NivelDeCobertura,
                                                                   ',','.')),
                           Limite_Garantia = as.numeric(str_replace(my.df$VL_LIMITE_GARANTIA,
                                                                    ',','.')),
                           Valor_Premio_Liq = as.numeric(str_replace(my.df$VL_PREMIO_LIQUIDO,
                                                                     ',','.')),
                           Pe_Tx =as.numeric(str_replace(my.df$PE_TAXA,',','.')),
                           Valor_Subvencao_Fed =as.numeric(str_replace(my.df$VL_SUBVENCAO_FEDERAL,
                                                                       ',','.')),
                           Nmro_Apolice = as.numeric(NR_APOLICE),
                            Nmro_Area_Total= as.numeric(str_replace(my.df$NR_AREA_TOTAL,
                                                  ',','.')),
                          geocode = as.numeric(my.df$CD_GEOCMU)) %>% 
      select(-c(NR_PROPOSTA,NR_PRODUTIVIDADE_ESTIMADA,NR_PRODUTIVIDADE_SEGURADA,
                NivelDeCobertura,VL_LIMITE_GARANTIA,VL_PREMIO_LIQUIDO,VL_SUBVENCAO_FEDERAL,
                NR_APOLICE,NR_AREA_TOTAL,CD_GEOCMU))
  }else{my.df = my.df%>%mutate(Nmro_Proposta = as.numeric(NR_PROPOSTA),
                               Nmro_Prod_Estimada = as.numeric(NR_PRODUTIVIDADE_ESTIMADA),
                               Nmro_Prod_Segurada = as.numeric(NR_PRODUTIVIDADE_SEGURADA),
                               NivelCobertura = as.numeric(str_replace(my.df$NivelDeCobertura,
                                                                       ',','.')),
                               Limite_Garantia = as.numeric(str_replace(my.df$VL_LIMITE_GARANTIA,
                                                                        ',','.')),
                               Valor_Premio_Liq = as.numeric(str_replace(my.df$VL_PREMIO_LIQUIDO,
                                                                         ',','.')),
                               Pe_Tx =as.numeric(str_replace(my.df$PE_TAXA,',','.')),
                               Valor_Subvencao_Fed =as.numeric(str_replace(my.df$VL_SUBVENCAO_FEDERAL,
                                                                           ',','.')),
                               Nmro_Apolice = as.numeric(NR_APOLICE),
                               Valor_Indenizacao =as.numeric(str_replace(my.df$VALOR_INDENIZAÇÃO,
                                                                         ',','.')),
                               Nmro_Area_Total= as.numeric(str_replace(my.df$NR_AREA_TOTAL,
                                                                       ',','.')),
                               geocode = as.numeric(my.df$CD_GEOCMU)) %>% 
    select(-c(NR_PROPOSTA,NR_PRODUTIVIDADE_ESTIMADA,NR_PRODUTIVIDADE_SEGURADA,
              NivelDeCobertura,VL_LIMITE_GARANTIA,VL_PREMIO_LIQUIDO,VL_SUBVENCAO_FEDERAL,
              NR_APOLICE,VALOR_INDENIZAÇÃO,NR_AREA_TOTAL,CD_GEOCMU))}
  return(my.df)
  
}

data_06_15 = change.numerical.values(my.df = data_06_15)
data_16_21 = change.numerical.values(my.df = data_16_21)
data_22 = change.numerical.values(my.df = data_22)
data_22 %>% glimpse

#Now, let's bind data from 06 to 21

data_06_21 = bind_rows(data_06_15,data_16_21)
#Save those files!
data_06_21 %>% 
  write_rds('./MiddleData/data_clean_06_21.rds')
data_22 %>% write_rds('./MiddleData/data_clean_22.rds')




