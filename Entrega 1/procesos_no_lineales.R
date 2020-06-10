
dong1 <- function(numero = 100) {

  # Esta función genera dos listas de números
  # Ambas están sujetas a cierta aleatoriedad en su generacción
  # e influyen una en la otra.

  x <- vector(mode = "numeric", length = numero)
  y <- vector(mode = "numeric", length = numero)

  x[1] <- 1
  y[1] <- 1

  for (i in 2:numero) {
    if (sample(2, 1) == 2) {
      m <- 1
    } else {
      m <- -1
    }
    x[i] <- 0.5 * x[i - 1] + 0.5 * y[i - 1] + m
    y[i] <- -0.5 * x[i - 1] + 0.5 * y[i - 1] + m
  }
  return(list(x = x[2:numero], y = y[2:numero]))
}

dong1(100)

# Podemos cambiar la función para alterar la función para elegir los valores iniciales

dong1.alt <- function(numero = 100, x_ini = 1, y_ini = 1) {

  # Esta función genera dos listas de números
  # Ambas están sujetas a cierta aleatoriedad en su generacción
  # e influyen una en la otra.

  x <- vector(mode = "numeric", length = numero)
  y <- vector(mode = "numeric", length = numero)

  x[1] <- x_ini
  y[1] <- y_ini

  for (i in 2:numero) {
    if (sample(2, 1) == 2) {
      m <- 1
    } else {
      m <- -1
    }
    x[i] <- 0.5 * x[i - 1] + 0.5 * y[i - 1] + m
    y[i] <- -0.5 * x[i - 1] + 0.5 * y[i - 1] + m
  }
  return(list(x = x[2:numero], y = y[2:numero]))
}

Inicio <- Sys.time()
dong1.alt(1000, 2, 2)
Sys.time() - Inicio

# Podemos aplicar a su vec la mejora de tiempo que se propone en el guion

dong1.alt.t <- function(numero = 100, x_ini = 1, y_ini = 1) {

  # Esta función genera dos listas de números
  # Ambas están sujetas a cierta aleatoriedad en su generacción
  # e influyen una en la otra.

  x <- vector(mode = "numeric", length = numero)
  y <- vector(mode = "numeric", length = numero)
  m <- vector(mode = "numeric", length = numero)

  x[1] <- x_ini
  y[1] <- y_ini

  m <- sample(c(-1, 1), numero, T)

  for (i in 2:numero) {
    x[i] <- 0.5 * x[i - 1] + 0.5 * y[i - 1] + m[i]
    y[i] <- -0.5 * x[i - 1] + 0.5 * y[i - 1] + m[i]
  }
  return(list(x = x[2:numero], y = y[2:numero]))
}

Inicio <- Sys.time()
dong1.alt.t(1000, 2, 2)
Sys.time() - Inicio

# Los experimentos no reflejan una mejora en el tiempo (incluso empeora)

# Podemos ampliar tambien el rango de valores donde se mdong1.alt.t.m <- function(numero = 100, x_ini = 1, y_ini = 1, m_range = 1) {

  # Esta función genera dos listas de números
  # Ambas están sujetas a cierta aleatoriedad en su generacción
  # e influyen una en la otra.

  x <- vector(mode = "numeric", length = numero)
  y <- vector(mode = "numeric", length = numero)
  m <- vector(mode = "numeric", length = numero)

  x[1] <- x_ini
  y[1] <- y_ini

  m <- sample(c(-m_range, m_range), numero, T)

  for (i in 2:numero) {
    x[i] <- 0.5 * x[i - 1] + 0.5 * y[i - 1] + m[i]
    y[i] <- -0.5 * x[i - 1] + 0.5 * y[i - 1] + m[i]
  }
  return(list(x = x[2:numero], y = y[2:numero]))
}


dong1.alt.t.m <- function(numero = 100, x_ini = 1, y_ini = 1, m_range = 1) {

  # Esta función genera dos listas de números
  # Ambas están sujetas a cierta aleatoriedad en su generacción
  # e influyen una en la otra.

  x <- vector(mode = "numeric", length = numero)
  y <- vector(mode = "numeric", length = numero)
  m <- vector(mode = "numeric", length = numero)

  x[1] <- x_ini
  y[1] <- y_ini

  m <- sample(c(-m_range, m_range), numero, T)

  for (i in 2:numero) {
    x[i] <- 0.5 * x[i - 1] + 0.5 * y[i - 1] + m[i]
    y[i] <- -0.5 * x[i - 1] + 0.5 * y[i - 1] + m[i]
  }
  return(list(x = x[2:numero], y = y[2:numero]))
}
