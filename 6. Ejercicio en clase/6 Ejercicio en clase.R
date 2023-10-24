#--------------------------------------------------------------------------------

# Tema:       Ejercicio en clase
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      septiembre 2023

#--------------------------------------------------------------------------


#0.  Entorno de trabajo

rm(list=ls()); graphics.off()    

paquetes <- c("tidyverse", "foreign", "factoextra", "ggplot2", "psych", "htmltools", "klaR")
for (i in paquetes) {if (!require(i, character.only = TRUE)) {install.packages(i);library(i, character.only = TRUE)} else {library(i, character.only = TRUE)}}


#1. Cargar base

# URL del archivo
url <- "https://raw.githubusercontent.com/jcms2665/UNAM-LCF-2023/main/6.%20Ejercicio%20en%20clase/datos.csv"
tmp_file <- tempfile()
download.file(url, tmp_file, method = "libcurl")
ejercicio=read.csv(tmp_file)



#2. Vamos a convertir las variables al formato que corresponde

ejercicio$sexo=as.factor(ejercicio$sexo)
ejercicio$edad=as.numeric(ejercicio$edad)
ejercicio$nivel_escolaridad=as.factor(ejercicio$nivel_escolaridad)
ejercicio$numero_trabajos=as.numeric(ejercicio$numero_trabajos)
ejercicio$atractivo=as.numeric(ejercicio$atractivo)
ejercicio$confiable=as.numeric(ejercicio$confiable)
ejercicio$sueldo=as.numeric(ejercicio$sueldo)
str(ejercicio)

# Si salen "Warning message" ignóralos

# La base está lista para ser utilizada


#3. Arreglo de la base de datos

# Filtrar variables
v_seleccionadas=c("...", "...", "....")
base_modelo=ejercicio[,v_seleccionadas]

v_seleccionadas=c("A", "B", "C", "D", "F")

