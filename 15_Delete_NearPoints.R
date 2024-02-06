# Esta parte da pesquisa foi realizada no Arcgis Pro, baseado nos resultados do script 14

# Utilizamos o teste average nearest neighbor para testar qual seria o valor médio experado de distancia entre os pontos
# A partir deste resultado, nos deletamos os pontos que passam desse "limite" da distancia.

# Esquema da análise:
# load data -> Calcular ANN global para cada cluster -> Separar entre Targets -> Realizar summarize nearby para cada target com a distancia experada pelo cluster global -> deletar pontos proximos -> calcular ANN para cada cluster por cada target