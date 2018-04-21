# importa bibilioteca
library(factoextra)

#Importa o dataset
voice <- read_csv("C:/dataset/voice.csv")

#Remove o Rotulo
data <- voice[,c(1:20)]

#Converte para matriz de númerico
data <- as.matrix(as.data.frame(lapply(data, as.numeric)))

#matriz de covariancia do dataset
data.cov <- cov(data)

#matriz de correlacao do dataset
data.cor <- cor(data)

#Executa PCA
data.pca <- princomp(data, cor=TRUE, scores=TRUE)

#Analise dos Componentes
summary(data.pca)

#matriz de covariancia pca
pca.cor <- cor(data.pca$scores)

#matriz de correlacao pca
pca.cor <- cor(data.pca$scores)

#Grafico Variancia x Componentes
fviz_eig(data.pca)

#Grafico de contribuicao das variaveis para a formacao dos componentes principais
fviz_pca_var(data.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
