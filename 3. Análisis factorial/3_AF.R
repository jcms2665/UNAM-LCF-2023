#--------------------------------------------------------------------------------
# Tema:       Analisis Factorial Exploratorio
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      septiembre 2023
# Datos:      pacientes.csv


#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#     2. Ajuste del modelo
#         2.1 Normalizacion 
#         2.2 Prueba Kaiser-Meyer-Olkin
#     3. Factorial
#     4. Interpretacion
#--------------------------------------------------------------------------


#0.  Entorno de trabajo

rm(list=ls())     
graphics.off()    

paquetes <- c("tidyverse", "foreign", "factoextra", "ggplot2", "psych")
for (i in paquetes) {if (!require(i, character.only = TRUE)) {install.packages(i);library(i, character.only = TRUE)} else {library(i, character.only = TRUE)}}



#1. Cargar base

url <- "https://raw.githubusercontent.com/jcms2665/UNAM-LCF-2023/main/3.%20An%C3%A1lisis%20factorial/pacientes.csv"
pacientes<-read.csv(url)

View(pacientes)

#2. Ajuste del modelo

#2.1 Normalización
norm01 <- function(x){(x-min(x))/(max(x)-min(x))}

pacientes_norm<-data.frame(apply(pacientes,2,norm01))


#2.2 Prueba Kaiser-Meyer-Olkin
#   La prueba de 0 a 1 e indica si las correlaciones entre variables son
#   suficientemente pequenas. Valores menores a 0.5 no deben ser incluidos

KMO(pacientes_norm)


#2.3 Numero de factores (Grafica de sedimentacion)
corr<-round(cor(pacientes_norm),2)
aucor=eigen(corr)
plot(1:5,aucor$values,type="l",xlab="Factores",ylab="Autovalores")


#3. Factorial
fit <- factanal(pacientes, 2, rotation="varimax")


#4. Interpretacion 
load <- fit$loadings[,1:2]
load


#5. Para la tarea

# Carga la base de datos "videos_youtube.csv" y nombrala "videos"

# Luego, tienes que sustituir el nombre de la base de datos, es decir, cambiar "pacientes" por "videos"

