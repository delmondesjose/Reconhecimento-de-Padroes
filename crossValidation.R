# importa bibilioteca
library(e1071)
library(xlsx)
library(nnet)

#---------------------------------------------------------------------------
# cross.partition -> Divide o dataset em k subconjuntos, com no maximo 1 
# elemento de diferança, e distribuição proporcional das classes
#
# Parametros: data -> dataset binario e balanceado
#             k -> Numero de particoes que serao geradas
#
# Retorno: partitions -> lista contendo os subconjuntos do dataset original
#--------------------------------------------------------------------------
cross.partition <- function( data , k=10 ){
  
  class1 <- unique(data$label)[1]   # Classe 1
  class2 <- unique(data$label)[2]   # Classe 2
  
  data.class1 <- subset( data , data$label == class1 )  #Subset contendo apenas as instancias da classe 1
  data.class2 <- subset( data , data$label == class2 )  #Subset contendo apenas as instancias da classe 2
  
  num.elem <- nrow(data) %/% k    # numero de instancias por conjunto (parte inteira)
  num.class <- num.elem %/% 2     # numero de instancias de cada classe
  
  partitions <- list()  # lista contendo os k conjuntos de instancias
  
  for(i in 1:k){
    
    instances.class1 <- sample( 1:nrow(data.class1) , num.class , replace=FALSE )     # pega num.class instancias da claase 1 aleatoriamente
    instances.class2 <- sample( 1:nrow(data.class2) , num.class , replace=FALSE )     # pega num.class instancias da claase 2 aleatoriamente
    
    partitions[[i]] <- rbind( data.class1[instances.class1,] , data.class2[instances.class2,] )   # cria particao contendo as instancias sorteadas, e adiciona à lista
    
    data.class1 <- data.class1[-instances.class1,]    # remove particoes já seleciondas do dataframe da classe 1 
    data.class2 <- data.class2[-instances.class2,]    # remove particoes já seleciondas do dataframe da classe 2 
  }
  
  i <- 1
  
  # distribuição de instancias restantes da classe 1
  while( nrow(data.class1) > 0 ){
    
    partitions[[i]] <- rbind( partitions[[i]] , data.class1[1,])
    data.class1 <- data.class1[-1,]
    i <- i+1
    
  }
  
  # distribuição de instancias restantes da classe 2
  while( nrow(data.class2) > 0 && i <= length(partitions) ){
    
    partitions[[i]] <- rbind( partitions[[i]] , data.class2[1,])
    data.class2 <- data.class2[-1,]
    i <- i+1
    
  }
  
  if( nrow(data.class2) > 0 ){
    i <- 1
    
    while( nrow(data.class2) > 0 ){
      
      partitions[[i]] <- rbind( partitions[[i]] , data.class2[1,])
      data.class2 <- data.class2[-1,]
      i <- i+1
      
    }
  }
  
  return(partitions)
  
}

#---------------------------------------------------------------------------
# training.partition-> cria particao de treinamento
#
# Parametros: list.part -> lista de datasets
#             part.test -> indice da particao de teste
#
# Retorno: data.training -> particao de treinamento
#--------------------------------------------------------------------------
training.partition<-function( list.part , part.test ){
  
  data.training <- data.frame()   # dataset de treinamento
  
  for(i in 1:length(list.part)){
    if(i != part.test){
      data.training <- rbind(list.part[[i]],data.training)
    }
  }
  return(data.training)
}

#---------------------------------------------------------------------------
# partition.print -> Imprimi lista de datasets
#
# Parametros: list.part -> lista de datasets
#--------------------------------------------------------------------------
partition.print <- function( list.data ){
  
  for(i in 1:length(list.data)){
    print(list.data[[i]])
  }
  
}

#----------------------------------------------------------------------------------
# measures.calc -> Calcula medidas de acuracia (erro, precisao e sensibilidade)
#
# Parametros: predict -> Resultado da predicao realizada sobre a amostra de teste
#             teste -> data frame de teste
#
# Retorno: measures -> Array contendo as medidas de erro, sensibilidade e precisão
#----------------------------------------------------------------------------------
measures.calc <- function(predict,teste){
 
  class1 <- unique(teste$label)[1]   # classe 1
  class2 <- unique(teste$label)[2]   # classe 2
  
  pos <- nrow(teste[teste$label==class1,])   # elementos positivos (classe male)
  neg <- nrow(teste[teste$label==class2,])   # elementos negativos (classe female)
  tot <- pos + neg # total de elementos
  tp  <- 0         # true positive
  tn  <- 0         # true negative
  fp  <- 0         # false positive
  fn  <- 0         # false negative
  
  predict <- as.data.frame(predict)
  
  # Rotulo numerico (tratamento utilizado pelas Redes Neurais)
  if(class(class1) == 'numeric'){
    for(i in 1:tot){
      if(teste[i,length(teste)] >= 0.5 && predict[i,1] >= 0.5)      tp <- tp+1   # soma true positive
      else if (teste[i,length(teste)] >= 0.5 && predict[i,1] < 0.5) fn <- fn+1   # soma false negative 
      else if (teste[i,length(teste)] < 0.5 && predict[i,1] < 0.5)  tn <- tn+1   # soma true negative
      else if (teste[i,length(teste)] < 0.5 && predict[i,1] >= 0.5) fp <- fp+1   # soma false positive
    }
  }
  # Rotulo string
  else{
    for(i in 1:tot){
      if(teste[i,length(teste)]=='male' && predict[i,1]=='male')            tp <- tp+1   # soma true positive 
      else if (teste[i,length(teste)]=='male' && predict[i,1]=='female')    fn <- fn+1   # soma false negative
      else if (teste[i,length(teste)]=='female' && predict[i,1]=='female')  tn <- tn+1   # soma true negative
      else if (teste[i,length(teste)]=='female' && predict[i,1]=='male')    fp <- fp+1   # soma false positive
    }
  }
  
  err <- (fp + fn) / tot  # calculo do erro
  sens <- tp / pos        # calculo da sensibilidade
  
  if(tp > 0) prec <- tp / (tp + fp)  # calculo da precisao
  else prec <- 0
  
  measures <- c(err,sens,prec)
  
  return(measures)
}

#----------------------------------------------------------------------------------------
# measures.mean -> Calcula a media das medidas (erro, precisao e sensibilidade)
#
# Parametros: measute -> lista de medidas
#
# Retorno: means -> Array contendo a media das medidas de erro sensibilidade e precisão
#----------------------------------------------------------------------------------------
measures.mean <- function(measures){
  
  err <- 0   # erro
  sens <- 0  # precisao
  prec <- 0  # sensibilidade
  
  for(i in 1:length(measures)){
    err <- err + measures[[i]][1]
    sens <- sens + measures[[i]][2]
    prec <- prec + measures[[i]][3]
  }
  
  err <- err / length(measures)    # Media do erro
  sens <- sens / length(measures)  # Media da sensibilidade
  prec <- prec / length(measures)  # Media da precisao
  
  means <- c(err,sens,prec)
  
  return(means)
}

#-----------------------------------------------------------------------------------------
# cross.valid.svm  -> executa cross validação para SVM
#
# Parametros: data    -> data frame
#             k       -> numero de particoes
#             cKernel -> kernel utilizado pela svm
#             nDegree -> parametro degree da svm
#             nGamma  -> parametro gamma da svm
#             nCoef0  -> parametro coef0 da svm
#             nCost   -> parametro cost da svm  
#
# Retorno: result -> linha do dataframe final contendo o resultado da validacao cruzada
#-----------------------------------------------------------------------------------------
cross.valid.svm <- function(data, k=10, cKernel, nDegree, nGamma, nCoef0, nCost){
  
  particoes <- cross.partition(data,k)   # Lista contendo as particoes para a validacao cruzada
  measures <- list()                     # Lista contendo os resultados (medidas) de cada execucao
  
  for(i in 1:length(particoes)){
    training <- training.partition(particoes,i)   # amostra de treinamento
    test <- particoes[[i]]                        # amostra de testes
    svm.model <- svm( label~. , data=training , kernel=cKernel , cost=nCost , gamma=nGamma , degree=nDegree , coef0=nCoef0 , type="C" )  # Executa treinamento
    svm.pred <- predict(svm.model, test[,-length(test)], type = "class")   #Executa teste
    measures[[i]] <- measures.calc( svm.pred , test)   # calcula medidas de acuracia
  }
  
  mean <- measures.mean(measures)   # calcula media dos resultados
  
  # cria linha do dataset de resultados
  result <- data.frame(kernel=cKernel,degree=nDegree,gamma=nGamma,coef0=nCoef0,cost=nCost,erro=mean[[1]],sensibilidade=mean[[2]],precisao=mean[[3]])
  
  return(result)
}

#---------------------------------------------------------------------------
# main.svm -> executa diversas chamadas a funcao cross.valid.svm, variando 
#             a parametrização da svm.
#
# Parametros: data -> data.frame
#             k    -> numero de particoes utilizadas na validacao cruzada
#--------------------------------------------------------------------------
main.svm <- function(data, k=10){
  
  data.result <- data.frame()
  
  aC0 <- c(0.1,0.5,1,5,10)  # parametro coef0
  aC1 <- c(0.1,0.5,1,5,10)  # parametro cost
  aD  <- c(0.1,0.5,1,5,10)  # parametro degree
  aG  <- c(0.1,0.5,1,5,10)  # parametro gamma
  
  # Kernel linear
  for(c1 in 1:length(aC1)){
    data.aux <- cross.valid.svm(data=data,k=k,cKernel='linear',nDegree=0,nGamma=0,nCoef0=0,nCost=aC1[c1])   # Executa valida cruzada
    data.result <- rbind(data.result,data.aux)
    print(sprintf("Kernel = linear, custo = %i.",c1))
  }
  
  # Kernel polynomial
  for(d in 1:length(aD)){
    for(g in 1:length(aG)){
      for(c0 in 1:length(aC0)){
        for(c1 in 1:length(aC1)){
          data.aux <- cross.valid.svm(data=data,k=k,cKernel='polynomial',nDegree=aD[d],nGamma=aG[g],nCoef0=aC0[c0],nCost=aC1[c1])  # Executa valida cruzada
          data.result <- rbind(data.result,data.aux)
          print(sprintf("Kernel = polynomial, custo = %i, degree = %i, gamma = %i, coef0 = %i.", c1, d, g, c0))
        }
      }
    }
  }
  
  # Kernel radial
  for(g in 1:length(aG)){
    for(c1 in 1:length(aC1)){
      data.aux <- cross.valid.svm(data=data,k=k,cKernel='radial',nDegree=0,nGamma=aG[g],nCoef0=0,nCost=aC1[c1])    # Executa valida cruzada
      data.result <- rbind(data.result,data.aux)
      print(sprintf("Kernel = radial, custo = %i, gamma = %i.", c1, g))
    }
  }
  
  # Kernel sigmoid
  for(g in 1:length(aG)){
    for(c0 in 1:length(aC0)){
      for(c1 in 1:length(aC1)){
        data.aux <- cross.valid.svm(data=data,k=k,cKernel='sigmoid',nDegree=0,nGamma=aG[g],nCoef0=aC0[c0],nCost=aC1[c1])    # Executa valida cruzada
        data.result <- rbind(data.result,data.aux)
        print(sprintf("Kernel = sigmoid, custo = %i, gamma = %i, coef0 = %i.", c1, g, c0))
      }
    }
  }
  
  return(data.result)
  
}

#---------------------------------------------------------------------------
# cross.valid.nn  -> executa cross validação para Rede Neural
#
# Parametros: data    -> data frame
#             k       -> numero de particoes
#             nSize   -> parametro size da rede neural
#             nRang   -> parametro rang da rede neural
#             nDecay  -> parametro decay da rede neural
#--------------------------------------------------------------------------
cross.valid.nn <- function(data, k=10, nSize, nRang, nDecay){
  
  particoes <- cross.partition(data,k)   # Lista contendo as particoes para a validacao cruzada
  measures <- list()                     # Lista contendo os resultados (medidas) de cada execucao
  
  for(i in 1:length(particoes)){
    training <- training.partition(particoes,i)   # amostra de treinamento
    test <- particoes[[i]]                        # amostra de testes
    nn.model <- nnet(training[,-length(training)],training$label,size=nSize,rang=nRang,decay=nDecay,maxit=500)  # Executa treinamento
    nn.pred <- predict(nn.model, test[,-length(test)])   #Executa teste
    measures[[i]] <- measures.calc( nn.pred , test)   # calcula medidas de acuracia
  }
  
  mean <- measures.mean(measures)   # calcula media dos resultados
  
  # cria linha do dataset de resultados
  result <- data.frame(size=nSize,rang=nRang,decay=nDecay,erro=mean[[1]],sensibilidade=mean[[2]],precisao=mean[[3]])
  
  return(result)
}

#---------------------------------------------------------------------------
# main.nn -> executa diversas chamadas da funcao cross.valid.nn, variando 
#            a parametrização da rede neural.
#
# Parametros: data -> data.frame
#             k    -> numero de particoes utilizadas na validacao cruzada
#--------------------------------------------------------------------------
main.nn <- function(data, k=10){
  
  # Conversao de rotulo para numerico
  data$label[data$label == 'male']   <- 1     
  data$label[data$label == 'female'] <- 0
  data$label <- as.numeric(data$label)
  
  data.result <- data.frame()
  
  nS <- c(2,4,6,8,10)  # parametro size
  nR <- c(0.2,0.4,0.6,0.8,1)  # parametro rang
  nD <- c(0.01,0.02,0.04,0.08,0.1)  # parametro decay
  
  for(s in 1:length(nS)){
    for(r in 1:length(nR)){
      for(d in 1:length(nD)){
        data.aux <- cross.valid.nn(data,k,nS[s],nR[r],nD[d])   # Executa validação cruzada
        data.result <- rbind(data.result,data.aux)
        print(sprintf("size = %i, rang = %i, decay = %i.", s, r, d))
      }
    }
  }
  
  return(data.result)
  
}

#---------------------------------------------------------------------------
# cross.valid.nb  -> executa cross validacao para Naive Bayers
#
# Parametros: data    -> data frame
#             k       -> numero de particoes
#--------------------------------------------------------------------------
cross.valid.nb <- function(data, k=10){
  particoes <- cross.partition(data,k)   # Lista contendo as particoes para a validacao cruzada
  measures <- list()                     # Lista contendo os resultados (medidas) de cada execucao
  
  for(i in 1:length(particoes)){
    training <- training.partition(particoes,i)   # amostra de treinamento
    test <- particoes[[i]]                        # amostra de testes
    nb.model <- naiveBayes(label~. , data=training)  # Executa treinamento
    nb.pred <- predict(nb.model, test[,-length(test)], type = "class")   #Executa teste
    measures[[i]] <- measures.calc( nb.pred , test)   # calcula medidas de acuracia
  }
  
  mean <- measures.mean(measures)   # calcula media dos resultados
  
  # cria linha do dataset de resultados
  result <- data.frame(erro=mean[[1]],sensibilidade=mean[[2]],precisao=mean[[3]])
  
  return(result)
}

#---------------------------------------------------------------------------
# main -> cahama validacao cruzada
#
# Parametros: data -> data.frame
#             k    -> numero de particoes utilizadas na validacao cruzada
#--------------------------------------------------------------------------
main.nb <- function(data, k=100){

  data.result <- data.frame()
  
  data.aux <- cross.valid.nb(data,k)
  data.result <- rbind(data.result,data.aux)
  
  return(data.result)

}