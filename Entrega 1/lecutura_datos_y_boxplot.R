
# Leemos los datos2
datos2 <- read.table("./Datos2.txt", header = T, as.is = T)
datos2

# Leemos los datos1
datos1 <- read.table("./Datos.txt", header = T, as.is = T)
datos1

# Podemos ver un histograma de la altura en los datos2
hist(datos2$Altura)

# Tambien podemos saber datos estadísticos sobre los mismos
summary(datos2$Altura)

# Obtenemos:
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  1.600   1.630   1.675   1.699   1.762   1.850


# Veamos que podemos hacer con BoxPlot, comenzamos con una sencilla ejecución

boxplot(datos2$Peso)

# Podemos tocar los parámetros para obtener mejores resultados

boxplot(datos2$Peso,
  main = "Peso en Datos2",
  xlab = "Peso en kg",
  col = "orange",
  border = "brown",
  horizontal = TRUE,
  notch = TRUE
)


# Podemos construir la distribucion normal que tenga la misma media y desviacion tipica que los datos que tenemos

peso_norm <- rnorm(200, mean = mean(datos2$Peso, na.rm = TRUE), sd = sd(datos2$Peso, na.rm = TRUE))

# Podemos pintar tanto el original como la nueva version

boxplot(datos2$Peso, peso_norm,
  main = "Multiples BoxPlot - Peso en Datos2",
  names = c("Original", "normal"),
  las = 2,
  col = c("orange", "red"),
  border = "brown",
  horizontal = TRUE,
  notch = TRUE
)

# Podemos mostrar tambien todos los datos en un mismo boxplot

boxplot(datos2$Peso, datos2$Altura, datos2$Edad
  main = "Multiples BoxPlot - Peso, Altura y Edad",
  names = c("Peso", "Altura", "Edad"),
  las = 2,
  col = c("green", "red", "blue"),
  border = "brown",
  horizontal = TRUE,
  notch = TRUE
)
