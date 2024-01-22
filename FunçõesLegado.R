#############################################
# FUNÇÃO LEGADO VIF                         #
#############################################
vif_spatial2 <- function(n_sample, rast_files){
    #Iniciando o sample das variaveis
    if(nrow(rast_files)*ncol(rast_files) < n_sample){
        stop("Numero de amostras é maior que a quantidade de células do raster")
        
    }else if (n_sample < nlyr(stack_raster)) {
       stop("Numero de amostras é menor que a quantidade de variaveis. R² explodirá ao infinito")
    }else{
        print('Iniciando sample')
        data_frame_var_samples <- terra::spatSample(rast_files,size = n_sample, method = 'random', na.rm=T,as.df=T)
    }
    #Criando matriz de correlação para auxiliar na decisao:
    cor_variables = cor(data_frame_var_samples)    
    
     print('Iniciando VIF')
    vif_value <- list()
    for(i in 1:ncol(data_frame_var_samples)){
        vif_value[[i]] <- 1/(1 - summary(lm(data_frame_var_samples[,i]~ ., data = data_frame_var_samples[-i]))$adj.r.squared)       
}
       
    print('Feito!')
    return (list(sample_values = data_frame_var_samples,vif_values = vif_value, correlation_variables = cor_variables))
}
tic()
vif_test = vif_spatial2(n_sample = 1000,rast_files =stack_raster )
toc()
