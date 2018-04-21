# importa bibilioteca
library(FSelector)

#Importa o dataset
voice <- read_csv("C:/dataset/voice.csv")

#Ajusta Dataset
data <- voice[,c(21,1:20)]

#Executa Relief
weights <- relief(data, neighbours.count = 5, sample.size = 3168)

#imprime resultados
print(weights)

#Selione 5 melhores caracteristicas
cutoff.k(weights,5)
