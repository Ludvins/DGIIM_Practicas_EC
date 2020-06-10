
# Escribimos las funciones que hacen la suma de los cuadrados y los cubos
# de los elementos del vector x

# Devuelve el resultado en una lista

sumpot <- function(x = NA) {
  return(list(cuadrados = sum(x^2), cubos = sum(x^3)))
}

# Probamos la función
sumplot(1:10)

# Escribimos la versión de la misma función que hace la suma de las potencias que le indiquemos

sumpot <- function(x, potencias = 2:3) {
  L <- length(potencias)
  resultado <- vector("list", length = L)

  for (i in 1:L) {
    resultado[[i]] <- sum(x^potencias[i])
  }

  return(resultado)
}

# Pruebas

sumplot(1:10)
sumplot(1:10, c(3, 5, 7))
