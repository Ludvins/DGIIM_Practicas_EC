
# En este ejemplo tenemos dos funciones que nos permiten construir una cuadrícula
# utilizando como función la distancia al origen de coordenadas

# La primera de ellas la construye realizando "manualmente" todas las iteraciones

distancia.origen <- function(x, y) {
  puntos <- matrix(-1, length(x), length(y))
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      puntos[i, j] <- sqrt(x[i]^2 + y[j]^2)
    }
  }
  return(puntos)
}

ptm <- proc.time()
distancia.origen(1:20, -10:10)
proc.time() - ptm

# La segunda de las funciones, hace uso de "outer" para utilizar una sintexis mas compacta además
# de aumentar su eficiencia.
# Para medir esto último utilizamos proc.time()

distancia.ORIGEN <- function(x, y) {
  outer(x, y, function(x, y) sqrt(x^2 + y^2))
}

ptm <- proc.time()
distancia.ORIGEN(1:20, -10:10)
proc.time() - ptm

# Con persp podemos dibujar la malla que forma la cuadricula.

persp(distancia.ORIGEN(1:20, -10:10))
