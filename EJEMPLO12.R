library(tidytext)
library(dplyr)

texto <- "Mia domo, estas la plej granda kaj bela el ciuj blankaj domoj en mia strato, F"

#Extraer Tokens y transforma en minzscula

extraeTokens <- function(texto){
  
  return((data_frame(text = texto) %>%  unnest_tokens(word, text))$word)
  
}


#Contar caracteres por tokens: Por construccisn de la anterior funcisn, esto lo da la funcisn que existe
#en R base denominada nchar

nchar(extraeTokens(texto))

#Se puede contar, aunque no es necesario el nzmero de tokens en el texto. Nstese que la "," se elimina
length(extraeTokens(texto))

#Determinar el valor del zltimo caracter del token
extraeUltLetra <- function(texto){

  vtexto = extraeTokens(texto)
  #Decisor Vectorizado
  TokenFinal = ifelse(nchar(extraeTokens(vtexto)) <= 1, "#", 
                      substr(vtexto,nchar(extraeTokens(vtexto)), nchar(extraeTokens(vtexto))))
  
  return (TokenFinal)
  
}


#Determinar el valor del penzltimo caracter del token
extraePenUltLetra <- function(texto){
  
  vtexto = extraeTokens(texto)
  #Decisor Vectorizado
  TokenFinal = ifelse(nchar(extraeTokens(vtexto)) <= 2, "#", 
                      substr(vtexto,(nchar(extraeTokens(vtexto))-1), (nchar(extraeTokens(vtexto))-1)))
  
  
  return (TokenFinal)
  
}

#Determinar el valor del Antpenzltimo caracter del token
extraeAntPenUltLetra <- function(texto){
  
  vtexto = extraeTokens(texto)
  #Decisor Vectorizado
  TokenFinal = ifelse(nchar(extraeTokens(vtexto)) <= 3, "#", 
                      substr(vtexto,(nchar(extraeTokens(vtexto))-2), (nchar(extraeTokens(vtexto))-2)))
  
  
  return (TokenFinal)
  
}

#Determinar el valor del Ante - Antpenzltimo caracter del token
extraeAntAntPenUltLetra <- function(texto){
  
  vtexto = extraeTokens(texto)
  #Decisor Vectorizado
  TokenFinal = ifelse(nchar(extraeTokens(vtexto)) <= 4, "#", 
                      substr(vtexto,(nchar(extraeTokens(vtexto))-3), (nchar(extraeTokens(vtexto))-3)))
  
  
  return (TokenFinal)
  
}

extraeUltLetra(texto)
extraePenUltLetra(texto)
extraeAntPenUltLetra(texto)
extraeAntAntPenUltLetra(texto) #Ejercicio, crear una funcisn con 2 parametros que resuma estas 4 en 1

#FUNCIONES EN ESPERANTO

#Cuenta Plurales
cuentaPlur <- function (texto){
  
  TokenFinalUlt <- extraeUltLetra(texto)
  
  DetectaPlural <- ifelse(extraeUltLetra(texto) == "j", 1, 0)

  #Excepcisn: comparador plej. Se puede introducir mas caso con %in%
  DetectaPlural <- ifelse(extraeTokens(texto) %in% c("kaj", "plej", "ciuj"), 0, DetectaPlural)
  
  return(sum(DetectaPlural))
  
}

cuentaPlur(texto)

#Cuenta Sustantivos:

  #Sustantivos en singular femenino
  cuentaSustSingFem <- function(texto){

    Detecta <- ifelse(extraeUltLetra(texto) == "o" & extraePenUltLetra(texto) == "n" & extraeAntPenUltLetra(texto) == "i", 1, 0)
    
    return(sum(Detecta))
    
  }
  
  cuentaSustSingFem(texto)

  texto2 <- "La homino estas belina"
  cuentaSustSingFem(texto2)
  
  #Sustantivos en singular masculino
  cuentaSustSingMasc <- function(texto){
    
    Detecta <- ifelse(extraeUltLetra(texto) == "o" & extraePenUltLetra(texto) != "n" & extraeAntPenUltLetra(texto) != "i", 1, 0)
    
    return(sum(Detecta))
    
  }
  
  cuentaSustSingMasc(texto)

  #Sustantivos en plural femenino
  cuentaSustPluFem <- function(texto){
    
    Detecta <- ifelse(extraeUltLetra(texto) == "j" & extraePenUltLetra(texto) == "o" & 
                        extraeAntPenUltLetra(texto) == "n" & extraeAntAntPenUltLetra(texto) == "i", 1, 0)
    
    return(sum(Detecta))
    
  }
  
  texto2 <- "La hominoj kaj infaninoj estas belinaj"
  cuentaSustPluFem(texto2)
  
  #Sustantivos en plural masculino
  cuentaSustPluMasc <- function(texto){
    
    Detecta <- ifelse(extraeUltLetra(texto) == "j" & extraePenUltLetra(texto) == "o" & 
                        extraeAntPenUltLetra(texto) != "n" & extraeAntAntPenUltLetra(texto) != "i", 1, 0)
    
    return(sum(Detecta))
    
  }
  
  cuentaSustPluMasc(texto)
  
  #Cuenta total sustantivos:
  
    #En Singular
    cuentaTotSustSingular <- function(texto){

      return(cuentaSustSingFem(texto) + cuentaSustSingMasc(texto))
      
    }
      
    
    #En Plural
    cuentaTotSustPlural <- function(texto) {
      
      return(cuentaSustPluFem(texto) + cuentaSustPluMasc(texto))
      
    }
      
    #En Total
    
    cuentaTotSust <- function(texto){
      
      return(cuentaTotSustSingular(texto) + cuentaTotSustPlural(texto))
      
    }
      
    cuentaTotSustSingular(texto)
    cuentaTotSustPlural(texto)
    cuentaTotSust(texto)
    
    