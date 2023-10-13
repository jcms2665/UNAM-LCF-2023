#--------------------------------------------------------------------------------
# Tema:       Analisis Factorial para variables categoricas
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      septiembre 2023
# Datos:      Latinobarometro_2018_Esp_R_v20190303.Rds

#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#     2. Preparacion de la base de datos
#     3. Correlacion policorica
#     4. Ajuste del modelo
#--------------------------------------------------------------------------


#0.  Entorno de trabajo

rm(list=ls())     
graphics.off()    

paquetes <- c("tidyverse", "foreign", "factoextra", "ggplot2", "psych", "httr")
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

# P15STGBSC.A --- Confianza en el Ejercito
# P15STGBSC.B --- Confianza en la Policia
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

# Filtrar por pais: Mexico y Brasil
dat<-latino%>%filter(as.numeric(IDENPA)==862 | as.numeric(IDENPA)==76)


# Filtrar variables
var<-c("P15STGBSC.A","P15STGBSC.B","P15STGBSC.C","P15STGBSC.D", "P15STGBSC.E")
dat<-dat[,var]
names(dat)<-c("Ejercito","Policia","Iglesia","Congreso","Gobierno")


# Quitar respuestas invalidas
dat[dat <=0] <- NA
dat<-dat%>%drop_na()

# Tabulados

table(dat$Ejercito)
table(dat$Iglesia)

# Proporciones

table(dat$Ejercito) %>% prop.table()
table(dat$Iglesia) %>% prop.table()


#3. Correlacion policorica 
poly_cor = polychoric(dat)
rho = poly_cor$rho
cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Correlacion tetracorica", show.legend = FALSE)


#4. Ajuste del modelo
poly_model = fa(dat, nfactor=2, cor="poly", fm="mle", rotate = "none")
poly_model$loadings
fa.diagram(poly_model)
