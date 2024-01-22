#########################################################
# Fazendo o download dos dados climáticos               #
#########################################################
library(geodata)
clim <- worldclim_country(var = 'bio', res = 0.5, download = F,country='Brazil', path = 'clim_data')
?worldclim_global
clim
plot(clim)
?geodata

get_clim_data = function(path_name=NULL,is.present=TRUE,country=NULL,var_type=NULL,res=NULL,
                         future_model_type=NULL,
                         scenario=NULL,
                         var_type_future=NULL,
                         res_future=NULL,
                         climate_change_time=NULL){
  if(is.present==T){
    clim_data = worldclim_country(var = var_type,
                                  res=res,country=country,
                                  path =path_name )
    return(clim_data)
  }
  
  else{
    
      
      cmip6_world(model = future_model_type,
                  ssp=scenario,time = climate_change_time,
                  var = var_type_future,
                  res =res_future,path =path_name  )
      file.rename(paste0(path_name,'/wc2.1_30s'), paste0(path_name,'/',future_model_type,'_',climate_change_time))
    
  }
  
  
}
#Para o presente
get_clim_data(path_name='./data_clim',is.present = T,
              country ='Brazil',
              var_type='bio',
              res =0.5)
#PAra o futuro
future_model_list=c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CanESM5", "CanESM5-CanOE", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", "HadGEM3-GC31-LL", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL")

for(model in future_model_list){
  get_clim_data(path_name='./data_clim/future_scenario',is.present = F,
                future_model_type = model,scenario ="126",
                var_type_future='bioc',
                res_future =0.5,
                climate_change_time='2021-2040')
}





