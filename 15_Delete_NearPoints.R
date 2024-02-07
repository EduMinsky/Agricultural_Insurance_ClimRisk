# Esta parte da pesquisa foi realizada no Arcgis Pro, baseado nos resultados do script 14

# Utilizamos o teste average nearest neighbor para testar qual seria o valor médio experado de distancia entre os pontos
# A partir deste resultado, nos deletamos os pontos que passam desse "limite" da distancia.

# Esquema da análise:
# calcular ANN -> Realizar summarize nearby pelo valor da distancia media -> deletar os pontos do teste summarize nearby que tem mais de 1 intersecao e que o valor target é zero -> spatial join (one to many) -> deleta pontos onde essa contagem de pontos é maior que 1, o join count é igual a 1 e target é zero -> seleciona os pontos onde o JoinCount é 1