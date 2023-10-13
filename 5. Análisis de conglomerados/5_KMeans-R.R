#--------------------------------------------------------------------------------
# Tema:       Analisis de conglomerados: k-medias
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      septiembre 2023
# Datos:      Latinobarometro_2018_Esp_R_v20190303.Rds

#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#     2. Arreglo de la base de datos
#     3. Algoritmo de k-medias con 3 grupos
#     4. Pegar grupos a la base original
#     5. Interpretacion
#--------------------------------------------------------------------------


#0.  Entorno de trabajo

rm(list=ls())     
graphics.off()    

rm(list=ls())     
graphics.off()    

paquetes <- c("tidyverse", "foreign", "factoextra", "ggplot2", "psych", "htmltools", "klaR")
for (i in paquetes) {if (!require(i, character.only = TRUE)) {install.packages(i);library(i, character.only = TRUE)} else {library(i, character.only = TRUE)}}


#1. Cargar base

# URL del archivo
url <- "https://github.com/jcms2665/UNAM-LCF-2023/raw/main/4.%20An%C3%A1lisis%20factorial%20con%20datos%20categ%C3%B3ricos/Latinobarometro_2018_Esp_R_v20190303.rds"

# Descargar el archivo a un archivo temporal
tmp_file <- tempfile()
download.file(url, tmp_file, method = "libcurl")

# Leer el archivo .rds
latino <- readRDS(tmp_file)


# Variables:

# P15STGBSC.A --- Confianza en las Fuerzas Armadas
# P15STGBSC.B --- Confianza en la Polic?a
# P15STGBSC.C --- Confianza en la Iglesia
# P15STGBSC.D --- Confianza en el Congreso
# P15STGBSC.E --- Confianza en el Gobierno

# Respuestas:
# 1.- Mucha confianza
# 2.- Algo de confianza
# 3.- Poca confianza
# 4.- Ninguna confianza
# -1-.- No sabe
# -2-.- No responde
# -4-.- No preguntada


#2. Arreglo de la base de datos

# Filtrar paises: Argentina, Bolivia, Brasil
dat<-latino%>%filter(as.numeric(IDENPA)==32 | as.numeric(IDENPA)==68 | as.numeric(IDENPA)==76)


# Filtrar variables
var<-c("REEDAD","SEXO","IDENPA","P15STGBSC.A","P15STGBSC.B","P15STGBSC.C","P15STGBSC.D", "P15STGBSC.E")
dat1<-dat[,var]
names(dat1)<-c("Edad","Sexo","Pais","Fuerzas Armadas","Policia","Iglesia","Congreso","Gobierno")



# Quitar respuestas inv?lidas
dat1[dat1 <=0] <- NA
dat1<-dat1%>%drop_na()


# Etiquetar variables (para identificar a la unidad de analisis: personas)
dat1$Edad<-factor(dat1$Edad,levels = c(1,2,3,4), labels = c("16-25 anos","26-40 anos","41-60 anos","60 y mas"))
dat1$Sexo<-factor(dat1$Sexo,levels = c(1,2), labels = c("Hombre","Mujer"))
dat1$Pais<-factor(dat1$Pais,levels = c(32,68,76), labels = c("Argentina","Bolivia","Brasil"))
View(dat1)

# Filtrar variables para el analisis
dat<-dat1[,4:8]
View(dat)

#3. Algoritmo de k-medias con 3 grupos
fit <-kmodes(dat, 3)


#4. Pegar grupos a la base original
dat.grupos <- data.frame(dat1, fit$cluster)


#5. Interpretacion
View(dat.grupos)
table(dat.grupos$fit.cluster,dat.grupos$Edad)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$Sexo)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$Pais)%>%prop.table(1)%>%`*`(100)%>%round(1)



