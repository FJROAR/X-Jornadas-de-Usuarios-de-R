library(readr)
library(dplyr)
library(stringr)

PATH = "D:/FJRA/NLP/TALLER_R/PRACTICAS/"
fichero = "DatosClasificacion.csv"
Datos <- read_delim(paste0(PATH, fichero), delim = ";")

df = data_frame(text = tolower(Datos$Mensaje))

#Matriz de Términos
CreaMatrizTerm <- function(filas, columnas){
  
  matrizTerm = matrix(nrow = length(filas), ncol = length(columnas))
  
  for(i in 1:length(filas)){
    
    matrizTerm[i, ] <- as.numeric(str_detect(tolower(filas[i]) , columnas))
  }
  
  matrizTerm <- data.frame(matrizTerm)  
  
  return(matrizTerm)
}


#Estimación de la Matriz

Keywords = c("móvil", "roto", "cobertura", "lent", "pack", "terminal", "internet", "wifi")

#Variables Explicativas
matrizTerm <- CreaMatrizTerm(df$text, Keywords)
names(matrizTerm) <-  Keywords

#Variable Explicada Nivel 1
N1 = as.factor(Datos$Categoria1)

datAnalisis <- cbind(matrizTerm, N1)

#Modelo1
library(nnet)

Modelo1 <- multinom(N1 ~., data = datAnalisis)
summary(Modelo1)

#Predicción

prepDatosPred <- function(texto, Keywords){

  Vector = CreaMatrizTerm(texto, Keywords)
  names(Vector) <- Keywords
  
  return(Vector)  
  
}

v1 <- prepDatosPred("El pack móvil se me ha roto", Keywords)
v2 <- prepDatosPred("La Wifi no me va bien", Keywords)
v3 <- prepDatosPred("El pack llegó roto y los datos van muy lentos", Keywords)
v4 <- prepDatosPred("El pack está bien pero los datos van muy lentos", Keywords)


predict(Modelo1, v1)
predict(Modelo1, v2)
predict(Modelo1, v3, type = "class")
predict(Modelo1, v3, type = "probs")
predict(Modelo1, v4, type = "class")
predict(Modelo1, v4, type = "probs")

#Modelo2
library(randomForest)

Modelo2 <- randomForest(N1 ~., data = datAnalisis)

v1 <- prepDatosPred("El pack móvil se me ha roto", Keywords)
v2 <- prepDatosPred("La Wifi no me va bien", Keywords)
v3 <- prepDatosPred("El pack llegó roto y los datos van muy lentos", Keywords)
v4 <- prepDatosPred("El pack está bien pero los datos van muy lentos", Keywords)


predict(Modelo2, v1)
predict(Modelo2, v2)
predict(Modelo2, v3, type = "response")
predict(Modelo2, v3, type = "prob")
predict(Modelo2, v4, type = "response")
predict(Modelo2, v4, type = "prob")