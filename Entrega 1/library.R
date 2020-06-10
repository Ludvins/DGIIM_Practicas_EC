
# Para esta prueba necesitamos instalar el paquete data.table
# install.packages("data.table")

library(data.table)

# Desde la ayuda podemos obtener los enlaces a su repositorio de github y a aquellas opciones que oferce
library(help = data.table)

# Este paquete nos permitirá leer los datos que tenemos directamente desde su enlace

datos <- fread("http://www.ugr.es/local/andresgc/Datos.txt")
head(datos)
