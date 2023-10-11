#--------------------------------------------------------------------------------
# Tema:       Analisis de Componentes Principales
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      septiembre 2023
# Datos:      calificacion.csv
# Github:     https://github.com/jcms2665/UNAM-2021-Multivariado


#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#         1.1 Opcion 1
#         1.2 Opcion 2
#     2. Normalizacion
#     3. Ajuste del modelo
#         3.1 Grafico de sedimentacion 
#         3.2 Componentes principales
#     4. Correlacion: CP vs Datos originales

#--------------------------------------------------------------------------


#0.  Entorno de trabajo

rm(list=ls())     
graphics.off()    

library(foreign)
library(factoextra)
library(dplyr)
library(knitr)


#1. Cargar base

#1.1 Opcion 2

setwd(".....")
horario<-read.csv("horario.csv", sep=",",header = TRUE)
View(horario)

#2. Ajuste del modelo

#2.1 Correlaciones
cor(horario)%>%round(2)

#2.2 Normalizacion
norm01 <- function(x){(x-min(x))/(max(x)-min(x))}

horario_norm<-data.frame(apply(horario,2,norm01))

# Una buena practica es analizar el comportamiento de las variables
apply(horario_norm, 2, min)%>%round(2)
apply(horario_norm, 2, mean)%>%round(2)
apply(horario_norm, 2, max)%>%round(2)

#2.3 Analisis de componentes principales
acp<-prcomp(horario_norm)
acp


#3. Resultados

#3.1 Grafico de sedimentacion (Varianza)
screeplot(acp,type="lines")









