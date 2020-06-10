
CuatroAses <- function(Mostrar = F, Maximo = 1000) {

  # Pone los cuatro ases en 0
  # Saca una carta de la bajara (52)
  # Suma uno al contador
  # Si no es un as (1) vuelve al principio del ciclo
  # El as correspondiente se pone en 1
  # Si los cuatro ases son 1 termina el ciclo
  Extracciones <- 0
  Resultado <- 1:Maximo
  Ases <- c(0, 0, 0, 0)

  repeat{
    if (Maximo <= Extracciones) {
      if (Mostrar) {
        cat("No he podido obtener cuatro ases en ", Extracciones, "extracciones. \n")
      }

      return(list(E = NA, R = Resultado, Conseguido = F))
    }
    Extracciones <- Extracciones + 1
    SacoUna <- sample(52, 1)

    Resultado[Extracciones] <- SacoUna

    if (SacoUna %% 13 != 1) {
      next
    }
    Ases[(SacoUna - 1) %/% 13 + 1] <- 1

    if (sum(Ases) == 4) {
      break
    }
  }
  length(Resultado) <- Extracciones

  if (Mostrar) {
    cat("He necesitado ", Extracciones, " extracciones para obtener cuatro ases. \n")
  }

  return(list(E = Extracciones, R = Resultado, Conseguido = T))
}

# Ejecuciones de prueba

CuatroAses(, 10)

CuatroAses(T, 10)

CuatroAses()

CuatroAses(T)


DistriAses <- function(n = 5, Maximo = 1000) {

  # Ejecuta CuatroAses n veces y devuelve las extracciones de cada una de ellas

  Saco <- vector(length = n)
  for (i in 1:n) {
    Saco[i] <- CuatroAses(F, Maximo)$E
  }

  return(Saco)
}

# Pruebas DistriAses
res <- DistriAses(100)
res
summary(res)
hist(res)


CuatroAses.Sin <- function(Mostrar = F, Maximo = 1000) {

  # Pone los cuatro ases en 0
  # Obtiene una permutacion de las cartas de la baraja
  # Si no es un as (1) pasa a la siguiente carta
  # Suma 1 al contador de ases
  # Si hay cuatro ases termina el ciclo

  Ases <- 0
  Resultado <- sample(52)

  for (i in 1:52) {
    if (Resultado[i] %% 13 != 1) {
      next
    }
    Ases <- Ases + 1
    if (Ases == 4) {
      break
    }
  }

  if (Mostrar) {
    cat("He necesitado ", i, " extracciones para obtener cuatro ases\n")
  }


  return(list(E = i, R = Resultado[1:i]))
}

# Pruebas CuatroAses.Sin
CuatroAses.Sin()

CuatroAses.Sin(T)

DistriAses.Sin <- function(n = 5) {
  Saco <- vector(length = n)
  for (i in 1:n) {
    Saco[i] <- CuatroAses.Sin()$E
  }

  return(Saco)
}

# Pruebas DistriAses.Sin

res <- DistriAses.Sin(2000)
res
summary(res)
hist(res)


CuatroAses.Nombres <- function(Mostrar = F, Maximo = 1000) {

  # Pone los cuatro ases en 0
  # Saca una carta de la baraja (52)
  # Suma uno al contador
  # Si no es un as (1) vuelve al principio del ciclo
  # El as correspondiente se pone en 1
  # Si los cuatro ases son 1 termina el ciclo

  Palos <- c("Oros", "Copas", "Espadas", "Bastos")
  Cartas <- c("As", "Dos", "Tres", "Cuatro", "Cinco", "Seis", "Siete", "Ocho", "Nueve", "Diez", "Sota", "Caballo", "Rey")
  Extracciones <- 0
  Resultado <- 1:Maximo
  Nombres <- 0
  Ases <- c(0, 0, 0, 0)

  repeat {
    Extracciones <- Extracciones + 1
    if (Maximo <= Extracciones) {
      if (Mostrar) {
        cat("No he podido obtener cuatro ases en ", Extracciones, "extracciones. \n")
      }
      return(list(E = NA, R = Resultado, N = Nombres, Conseguido = F))
    }
    SacoUna <- sample(52, 1)
    Resultado[Extracciones] <- SacoUna

    Palo <- ((SacoUna - 1) %/% 13) + 1
    Carta <- ((SacoUna - 1) %% 13) + 1

    Nombres[Extracciones] <- paste(Cartas[Carta], " de ", Palos[Palo])

    if (SacoUna %% 13 != 1) {
      next
    }

    Ases[(SacoUna - 1) %/% 13 + 1] <- 1

    if (sum(Ases) == 4) {
      break
    }
  }
  length(Resultado) <- Extracciones

  if (Mostrar) {
    cat("He necesitado ", Extracciones, " extracciones para obtener cuatro ases. \n")
  }
  return(list(E = NA, R = Resultado, N = Nombres, Conseguido = T))
}

CuatroAses.Nombres(T, 10)


# Función de prueba, tira dados y va acumulando los valores obtenidos hasta alcanzar el objetivo,
# en caso de pasarse considera que no lo ha conseguido

TirarDados <- function(Objetivo = 10, Mostrar = F, Maximo = 1000) {

  # Inicializamos las variables
  Tiradas <- 0
  Resultado <- 1:Maximo
  Acumulado <- 0

  repeat{
    # Si supera el numero maximo de tiradas sin llegar al objetivo, revolvemos que no
    # lo ha conseguido

    if (Maximo <= Tiradas) {
      if (Mostrar) {
        cat("No he podido llegar al objetivo en ", Tiradas, " tiradas. \n")
      }

      return(list(E = NA, R = Resultado, Conseguido = F))
    }
    # Hacemos una nueva tirada
    Tiradas <- Tiradas + 1
    Tirada <- sample(6, 1)

    # Actualizamos los valores obtenidos
    Resultado[Tiradas] <- Tirada
    Acumulado <- Acumulado + Tirada

    # Si alcanzamos el objetivo devolvemos exito y todos los valores
    if (Acumulado == Objetivo) {
      if (Mostrar) {
        cat("He necesitado ", Tiradas, " tiradas para alcanzar el objetivo. \n")
      }
      length(Resultado) <- Tiradas
      return(list(E = Tiradas, R = Resultado, Conseguido = T))
    }
    # Si nos pasamos devolvemos que no lo hemos conseguido y el vector de tiradas
    if (Acumulado > Objetivo) {
      if (Mostrar) {
        cat("En ", Tiradas, " tiradas me he pasado del objetivo. \n")
      }
      length(Resultado) <- Tiradas
      return(list(E = Tiradas, R = Resultado, Conseguido = F))
    }
  }
}

# Pruebas de la función
TirarDados()
TirarDados(100)


# Función que llama a la función un número de veces
Distri.Dados <- function(n = 5) {
  Saco <- vector(length = n)
  for (i in 1:n) {
    Saco[i] <- TirarDados(100)$E
  }

  return(Saco)
}

# Pruebas Distri.Dados
res <- Distri.Dados(2000)
res
summary(res)
hist(res)
