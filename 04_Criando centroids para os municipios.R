###### This script is to create centroid of municipalities ######
# The reason for that is that we need to spacialize the data without coordinates
list.files('./SupportData/Municipios/')
municipio = st_read('./SupportData/Municipios/BR_Municipios_2022.shp')
#Checking the centroids, before saving it
st_point_on_surface(x =municipio ) %>% st_geometry() %>% plot
mun_point_surface=st_point_on_surface(x =municipio )
#Saving
mun_point_surface%>%st_write('./SupportData/Municipios/BR_Municipios_2022_PointSurface.shp')






