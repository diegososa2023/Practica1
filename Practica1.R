############################################################################################################
################################################ PRACTICA 1 ################################################
############################################################################################################

#################### ALUMNOS #########################
##### - DIEGO SOSA ALVA                              #
##### - FELIPE VASQUEZ VASQUEZ
#
######################################################

# Realizamos la instalación de las librerías

install.packages("readr")
library(readr)
install.packages("stringr")
library(stringr)
install.packages("tidyr")
library(tidyr)
install.packages("dplyr")
library(dplyr)

# Importamos los datos que se encuentran en el archivo "epa-http.csv" y no colocamos la primera fila como nombre de las columnas
epa_http <- read_table("D:/MAESTRIA/3er_Ciclo/3_DATA SCIENCE APLICADO A LA CIBERSEGURIDAD/epa-http.csv",col_names = FALSE)
# Nos muestra los resultados de la importación de la tabla
View(epa_http)
# Cambiamos el nombre de las columnas por los siguientes nombres
names(epa_http) <- c("IPs", "Tiempo", "Tipo", "URL", "Protocolo", "Codigo", "Byte")
# Quitamos el simbolo de comilla doble  "  de la columna Tipo
epa_http$Tipo <- substring(epa_http$Tipo, 2)
# Quitamos el simbolo de comilla doble  "  de la columna Protocolo
epa_http$Protocolo <- substr(epa_http$Protocolo, 1, nchar(epa_http$Protocolo)-1)

##############
# PREGUNTA 1 #
##############

# 1.1 Cuales son las dimensiones del dataset cargado (número de filas y columnas)

# Usando el comando DIM no muestra el tamaño del arreglo o de la tabla en filas y columnas
dim(epa_http)

# RESULTADO :
# [1] 47748     7


# 1.2 Valor medio de la columna Bytes

# Cambiamos la columna Byte a tipo numerico para poder calcular el promedio
epa_http$Byte <- as.numeric(epa_http$Byte)
# Calculamos el promedio de la columna Byte y le indicamos que en la columna hay valores NA
mean(epa_http$Byte, na.rm=TRUE)

# RESULTADO :
# [1] 7352.335


##############
# PREGUNTA 2 #
##############

# De las diferentes IPs de origen accediendo al servidor, ¿cuantas pertenecen a una IP claramente educativa (que contenga ".edu")?

# Usamos el siguiente comando que nos permite buscar en el texto .edu y lo va acumulando
sum(str_count(epa_http$IPs, "\\.edu"))

# RESULTADO :
# [1] 6524


##############
# PREGUNTA 3 #
##############

# De todas las peticiones recibidas por el servidor cual es la hora en la que hay mayor volumen de peticiones HTTP de tipo "GET"?

# Le damos formato tipo fecha a la columna Tiempo y lo alamacenamos en date
date <- as.POSIXct(epa_http$Tiempo, format = "[%d:%H:%M:%OS]")
# De la tabla date extraemos la hora
datehora <- format(date, format = "%H")
# A la tabla epa_http le agregamos la columna datehora
epa_http <- mutate(epa_http, column9= datehora)
# Procedemos con el conteo por hora
conteo <- table(datehora)
# Una vez realizado el conteo elegimos el maximo valor
cadena_mas_comun <- names(conteo)[which.max(conteo)]
# Leemos la ultima variable que contiene el resultado final
cat("La cadena de texto con más repeticiones en la columna de texto es:", cadena_mas_comun)

# RESULTADO :
# La cadena de texto con más repeticiones en la columna de texto es: 14


##############
# PREGUNTA 4 #
##############

# De las peticiones hechas por instituciones educativas (.edu), ¿Cuantos bytes en total se han transmitido, en peticiones de descarga de ficheros de texto ".txt"?

################################################# OPCION 1 #################################################
######## Filtrando columnas que contengan .edu y .txt

# Creamos una tabla filtrando en la culumna IPs todos los valores que contengan .edu
epa_edu <- epa_http %>% filter(grepl("\\.edu", IPs))
# A partir de la tabla anterior, creamos otra tabla filtrando en la columna URL todos lo valores que contengan .txt
epa_txt <- epa_edu %>% filter(grepl("\\.txt", URL))

# En la ultima tabla creada que solo tiene los .edu para IPs y .txt para URL, procedemos a sumar la columna Byte
sum(epa_txt$Byte, na.rm=TRUE)

# RESULTADO :
# [1] 2705408


################################################# OPCION 2 #################################################
######## Filtrando columnas que contengan .edu y y que terminen en .txt --- Esto porque los ficheros de texto terminan en .txt

# Creamos una tabla filtrando en la culumna IPs todos los valores que contengan .edu
epa_edu <- epa_http %>% filter(grepl("\\.edu", IPs))

# A partir de la tabla anterior, creamos otra tabla filtrando en la columna URL todos lo valores que terminan en .txt
epa_txt <- epa_edu %>% filter(grepl("\\.txt$", URL))

# En la ultima tabla creada que solo tiene los .edu para IPs y .txt para URL, procedemos a sumar la columna Byte
sum(epa_txt$Byte, na.rm=TRUE)

# RESULTADO :
# [1] 106806


##############
# PREGUNTA 5 #
##############

# Si separamos la petición en 3 partes (Tipo, URL, Protocolo), usando str_split y el separador " " (espacio), ¿cuantas peticiones buscan directamente la URL = "/"?

# Separamos las columnas hasta que encuentren un espacio en blanco
Tipo2 <- str_split(epa_http$Tipo, pattern = " ")
URL2 <- str_split(epa_http$URL, pattern = " ")
Protocolo2 <- str_split(epa_http$Protocolo, pattern = " ")

# Las valiables anteriores se crearon como listas y debemos cambiarla a tipo vector
m <- unlist(Tipo2)
n <- unlist(URL2)
p <- unlist(Protocolo2)

# procedemos a agregar las columnas a la tabla epa_http
epa_http <- mutate(epa_http, column1= as.factor(m), column2 = as.factor(n), column3 = as.factor(p))

# Con la siguiente sentencia nos muestra el conteo de peticiones a la URL = "/"
summary(epa_http)

#IPs               Tiempo              Tipo               URL             Protocolo             Codigo           Byte        
#Length:47748       Length:47748       Length:47748       Length:47748       Length:47748       Min.   :200.0   Min.   :      0  
#Class :character   Class :character   Class :character   Class :character   Class :character   1st Qu.:200.0   1st Qu.:    231  
#Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median :200.0   Median :   1260  
#Mean   :227.1   Mean   :   7352  
#3rd Qu.:200.0   3rd Qu.:   3223  
#Max.   :501.0   Max.   :4816896  
#NA's   :5331     
#   column9          column1                              column2          column3     
# Length:47748       GET :46020   /icons/circle_logo_small.gif: 3203   HTTP/0.2:    1  
# Class :character   HEAD:  106   /                           : 2382   HTTP/1.0:47747  
# Mode  :character   POST: 1622   /logos/small_gopher.gif     : 1851                   
#                                 /logos/us-flag.gif          : 1817                   
#                                 /logos/small_ftp.gif        : 1815                   
#                                 /icons/book.gif             : 1800                   
#                                 (Other)                     :34880                   

################################################# METODO 1 #################################################

# Del summary obtenemos la respuesta
# RESPUESTA: 2382


################################################# METODO 2 #################################################

# Buscando aquella fila que sean igual a / y lo alamcenamos en un vector
datos_filtrados <- subset(epa_http, column3 == "/")

# Cuenta el numero de filas del nuevo vector
nrow(datos_filtrados)
# [1] 2382


##############
# PREGUNTA 6 #
##############

# Aprovechando que hemos separado la petición en 3 partes (Tipo, URL, Protocolo) ¿Cuantas peticiones NO tienen como protocolo "HTTP/0.2"?

# Con la siguiente sentencia nos muestra el conteo de peticiones que no tienen como protocolo "HTTP/0.2"
summary(epa_http)

#IPs               Tiempo              Tipo               URL             Protocolo             Codigo           Byte        
#Length:47748       Length:47748       Length:47748       Length:47748       Length:47748       Min.   :200.0   Min.   :      0  
#Class :character   Class :character   Class :character   Class :character   Class :character   1st Qu.:200.0   1st Qu.:    231  
#Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median :200.0   Median :   1260  
#Mean   :227.1   Mean   :   7352  
#3rd Qu.:200.0   3rd Qu.:   3223  
#Max.   :501.0   Max.   :4816896  
#NA's   :5331     
#   column9          column1                              column2          column3     
# Length:47748       GET :46020   /icons/circle_logo_small.gif: 3203   HTTP/0.2:    1  
# Class :character   HEAD:  106   /                           : 2382   HTTP/1.0:47747  
# Mode  :character   POST: 1622   /logos/small_gopher.gif     : 1851                   
#                                 /logos/us-flag.gif          : 1817                   
#                                 /logos/small_ftp.gif        : 1815                   
#                                 /icons/book.gif             : 1800                   
#                                 (Other)                     :34880                   

################################################# METODO 1 #################################################

# Del summary obtenemos la respuesta
# RESPUESTA: 47747

################################################# METODO 2 #################################################

# Almcenamos las filas que no son "HTTP/0.2" en el vector indices
indices <- grep("HTTP/0.2", epa_http$column3, invert = TRUE)

# Le pedimos el tamaño del vector indices
length(indices)
# [1] 47747

# RESPUESTA: 47747


################################################# METODO 3 #################################################

# 

sum(!grepl("^(HTTP/0.2)$", epa_http$column3))
