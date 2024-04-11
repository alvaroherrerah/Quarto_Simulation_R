
rm(list = ls())

source('funciones.R')



###################### VERSIÓN NO ALEATORIA #############################

move_no_aleatorio <- function(partida){
  if(partida$estado == 0 | partida$estado == 2) stop("esta partida ya ha terminado") # | partida$estado == 2 es un EMPATE
  
  # Seleccionamos una ficha disponible al azar (si queda más de una)
  if(length(partida$ficha$index[partida$ficha$disponible == 1]) > 1){
    ficha_rival <- as.numeric(partida$ficha[m[3], 1:4])
    
    for (i in 1:length(partida$ficha$altura)) {
      if (ficha_rival[4] == 1){
        if (partida$ficha$tapa[i] == 0){
          if (partida$ficha$altura[i] == ficha_rival[1] && partida$ficha$color[i] == ficha_rival[2] && partida$ficha$forma[i] == ficha_rival[3]) {
            ficha_seleccionada <<- i
            break
          } 
        }
      } else if (ficha_rival[4] == 0){
        if (partida$ficha$tapa[i] == 1){
          if (partida$ficha$altura[i] == partida$ficha$altura[m[3]] && partida$ficha$color[i] == partida$ficha$color[m[3]] && partida$ficha$forma[i] == partida$ficha$forma[m[3]]) {
            ficha_seleccionada <<- i
            break
          } 
        }
      }
    }
  }
  
  # Seleccionamos la única ficha disponible (si solo queda una)
  if(length(partida$ficha$index[partida$ficha$disponible == 1]) == 1) 
    ficha_seleccionada <- partida$ficha$index[partida$ficha$disponible == 1]
  
  # Paramos de jugar en caso de empate
  if(length(partida$ficha$index[partida$ficha$disponible == 1]) == 0) {
    partida$estado == 2 # ESTADO DE EMPATE
    stop("Empate. La partida ha terminado.")
  }
  # montamos un df con todas las posiciones del tablero disponibles
  # y elegimos una aleatoriamente
  df <- data.frame(
    index = 1:16,
    r = rep(1:4, each = 4),
    c = rep(1:4, times = 4),
    d = as.vector(t(partida$tablero[ , , 1]))
  )
  # Una sola posición disponible
  if(length(which(is.na(df$d))) == 1)
    indice = df$index[is.na(df$d)]
  
  # Más de una posición disponible
  if(length(which(is.na(df$d))) > 1) {
    if (m[2] == 1){
      indice <- df[df$r == m[1] & df$c == 4, ]$index
    } else if (m[2] == 2) {
      indice <- df[df$r == m[1] & df$c == 3, ]$index
    } else if (m[2] == 3) {
      indice <- df[df$r == m[1] & df$c == 2, ]$index
    } else if (m[2] == 4) {
      indice <- df[df$r == m[1] & df$c == 1, ]$index
    }
  }
  
  # devolvemos la fila y la columna de la celda elegida aleatoriamente
  # junto con la ficha elegida
  m <<- c(df$r[indice], df$c[indice], ficha_seleccionada)
  
  # filas
  indica <- 0
  salir_bucle <- FALSE
  for (i in 1:4) {
    if (salir_bucle){
      break
    }
    for (j in 1:4) {
      if (sum(is.na(partida$tablero[,,i][j,])) == 1) {
        if ((mean(partida$tablero[,,i][j,], na.rm = TRUE) == 1)) {
          fila_nueva <<- data.frame(fila = j, dimension = i, valor = 1)
          indica <- 1
          salir_bucle <- TRUE
          break
        } else if ((mean(partida$tablero[,,i][j,], na.rm = TRUE) == 0)) {
          fila_nueva <<- data.frame(fila = j, dimension = i, valor = 0)
          indica <- 1
          salir_bucle <- TRUE
          break
        }
      }
    }
  }
  
  # columnas
  indica_2 <- 0
  salir_bucle <- FALSE
  for (i in 1:4) {
    if (salir_bucle){
      break
    }
    for (j in 1:4) {
      if (sum(is.na(partida$tablero[,,i][,j])) == 1) {
        if ((mean(partida$tablero[,,i][,j], na.rm = TRUE) == 1)) {
          columna_nueva <<- data.frame(columna = j, dimension = i, valor = 1)
          indica_2 <- 1
          salir_bucle <- TRUE
          break
        } else if ((mean(partida$tablero[,,i][,j], na.rm = TRUE) == 0)) {
          columna_nueva <<- data.frame(columna = j, dimension = i, valor = 0)
          indica_2 <- 1
          salir_bucle <- TRUE
          break
        }
      }
    }
  }
  
  # diagonales
  indica_3 <- 0
  for (i in 1:4) {
    if (sum(is.na(diag(partida$tablero[,,i]))) == 1) {
      if (is.nan(mean(diag(partida$tablero[,,i]), na.rm = TRUE)) == FALSE  && mean(diag(partida$tablero[,,i]), na.rm = TRUE) == 1) {
        posicion_NA <- which(is.na(diag(partida$tablero[,,i])))
        diagonal_principal <<- data.frame(posicion = posicion_NA, dimension = i, valor = 1)
        indica_3 <- 1
        break
      } else if (is.nan(mean(diag(partida$tablero[,,i]), na.rm = TRUE)) == FALSE && mean(diag(partida$tablero[,,i]), na.rm = TRUE) == 0) {
        posicion_NA <- which(is.na(diag(partida$tablero[,,i])))
        diagonal_principal <<- data.frame(posicion = posicion_NA, dimension = i, valor = 0)
        indica_3 <- 1
        break
      }
    }
  }
  
  indica_4 <- 0
  for (i in 1:4) {
    if (sum(is.na(diag(partida$tablero[,,i][,ncol(partida$tablero[,,i]):1]))) == 1){
      if (is.nan(mean(diag(partida$tablero[,,i][,ncol(partida$tablero[,,i]):1]), na.rm = TRUE)) == FALSE && mean(diag(partida$tablero[,,i][,ncol(partida$tablero[,,i]):1]), na.rm = TRUE) == 1) {
        posicion_fila_NA <- which(is.na(diag(partida$tablero[,,i][,ncol(partida$tablero[,,i]):1])))
        vector <- rev(diag(partida$tablero[,,i][,ncol(partida$tablero[,,i]):1]))
        posicion_columna_NA <- which(is.na(vector))
        diagonal_inversa <<- data.frame(posicion_fila = posicion_fila_NA, posicion_columna = posicion_columna_NA, dimension = i, valor = 1)
        indica_4 <- 1
        break
      } else if (is.nan(mean(diag(partida$tablero[,,i][,ncol(partida$tablero[,,i]):1]), na.rm = TRUE)) == FALSE && mean(diag(partida$tablero[,,i][,ncol(partida$tablero[,,i]):1]), na.rm = TRUE) == 0) {
        posicion_fila_NA <- which(is.na(diag(partida$tablero[,,i][,ncol(partida$tablero[,,i]):1])))
        vector <- rev(diag(partida$tablero[,,i][,ncol(partida$tablero[,,i]):1]))
        posicion_columna_NA <- which(is.na(vector))
        diagonal_inversa <<- data.frame(posicion_fila = posicion_fila_NA, posicion_columna = posicion_columna_NA, dimension = i, valor = 0)
        indica_4 <- 1
        break
      }
    }
  }
  
  if (indica == 1){
    columna <- which(is.na(partida$tablero[,,fila_nueva$dimension][fila_nueva$fila,]))
    for (i in 1:16) {
      if (partida$ficha[fila_nueva$dimension][i,] == fila_nueva$valor & 
          partida$ficha$disponible[i] == 1) {
        nueva_ficha_seleccionada <<- i
        break
      }
    }
    m <<- c(fila_nueva$fila, columna, nueva_ficha_seleccionada)
  }
  
  if (indica_2 == 1 & indica ==0){
    fila_c <- which(is.na(partida$tablero[,,columna_nueva$dimension][,columna_nueva$columna]))
    for (i in 1:16) {
      if (partida$ficha[columna_nueva$dimension][i,] == columna_nueva$valor & 
          partida$ficha$disponible[i] == 1) {
        nueva_ficha_seleccionada <<- i
        break
      }
    }
    m <<- c(fila_c, columna_nueva$columna, nueva_ficha_seleccionada)
  }
  
  if (indica_3 == 1 & indica == 0 & indica_2 == 0){
    fila_c <- diagonal_principal$posicion
    columna_c <- diagonal_principal$posicion
    for (i in 1:16) {
      if (partida$ficha[diagonal_principal$dimension][i,] == diagonal_principal$valor & 
          partida$ficha$disponible[i] == 1) {
        nueva_ficha_seleccionada <<- i
        break
      }
    }
    m <<- c(fila_c, columna_c, nueva_ficha_seleccionada)
  }
  
  
  if (indica_4 == 1 & indica == 0 & indica_2 == 0 & indica_3 == 0){
    fila_c <- diagonal_inversa$posicion_fila
    columna_c <- diagonal_inversa$posicion_columna
    for (i in 1:16) {
      if (partida$ficha[diagonal_inversa$dimension][i,] == diagonal_inversa$valor & 
          partida$ficha$disponible[i] == 1) {
        nueva_ficha_seleccionada <<- i
        break
      }
    }
    m <<- c(fila_c, columna_c, nueva_ficha_seleccionada)
  }
  
  
  return(m)
}


######################## VERSIÓN ALEATORIA ############################

move <- function(partida){
  if(partida$estado == 0 | partida$estado == 2) stop("esta partida ya ha terminado") # | partida$estado == 2 es un EMPATE
  
  # Seleccionamos una ficha disponible al azar (si queda más de una)
  if(length(partida$ficha$index[partida$ficha$disponible == 1]) > 1) 
    f <- sample(partida$ficha$index[partida$ficha$disponible == 1], 1)
  
  # Seleccionamos la única ficha disponible (si solo queda una)
  if(length(partida$ficha$index[partida$ficha$disponible == 1]) == 1) 
    f <- partida$ficha$index[partida$ficha$disponible == 1]
  
  # Paramos de jugar en caso de empate
  if(length(partida$ficha$index[partida$ficha$disponible == 1]) == 0) {
    partida$estado == 2 # ESTADO DE EMPATE
    stop("Empate. La partida ha terminado.")
  }
  
  
  
  # montamos un df con todas las posiciones del tablero disponibles
  # y elegimos una aleatoriamente
  df <- data.frame(
    index = 1:16,
    r = rep(1:4, each = 4),
    c = rep(1:4, times = 4),
    d = as.vector(t(partida$tablero[ , , 1]))
  )
  # Una sola posición disponible
  if(length(which(is.na(df$d))) == 1)
    i = df$index[is.na(df$d)]
  
  # Más de una posición disponible
  if(length(which(is.na(df$d))) > 1)
    i = sample(df$index[is.na(df$d)], 1)
  
  
  
  # devolvemos la fila y la columna de la celda elegida aleatoriamente
  # junto con la ficha elegida
  m <<- c(df$r[i], df$c[i], f)
  return(m)
}

###########################################################



###################### SIMULADOR ##########################

victoria_bot_random <- 0
victoria_bot_nr <- 0
empates <- 0

for (i in 1:100) {
  partida <- nuevojuego() 
  for (i in 1:16) {
    partida <- update(partida, move(partida))
    if (partida$estado == 0) {
      victoria_bot_random <- victoria_bot_random + 1
      break  
    } else if (partida$estado == 2) {
      empates <- empates + 1
      break
    }
    partida <- update(partida, move_no_aleatorio(partida))
    if (partida$estado == 0) {
      victoria_bot_nr <- victoria_bot_nr + 1
      break  
    } else if (partida$estado == 2) {
      empates <- empates + 1
      break
    }
  }  
}



# otro simulador

quien_gana <- function(algoritmo1, algoritmo2, repeticiones = 50) {
  victorias_algoritmo1 <- 0
  victorias_algoritmo2 <- 0
  empates <- 0
  barra_progreso <- txtProgressBar(min = 0, max = repeticiones, style = 3)
  for (i in 1:repeticiones) {
    partida <- nuevojuego()
    tryCatch({
      while (partida$estado == 1) {
        partida <- update(partida, algoritmo1(partida))
        if (partida$estado == 0) {
          victorias_algoritmo1 <- victorias_algoritmo1 + 1
        } else if (partida$estado == 2) {  
          empates <- empates + 1
        } else {
          partida <- update(partida, algoritmo2(partida))
          if (partida$estado == 0) {
            victorias_algoritmo2 <- victorias_algoritmo2 + 1
          } else if (partida$estado == 2) {  
            empates <- empates + 1
          }
        }
      }
    }, error = function(e) {})
    setTxtProgressBar(barra_progreso, i)
  }
  close(barra_progreso)
  cat("Victorias de", deparse(substitute(algoritmo1)), ":", victorias_algoritmo1, "\n")
  cat("Victorias de", deparse(substitute(algoritmo2)), ":", victorias_algoritmo2, "\n")
  cat("Empates:", empates, "\n")
}

quien_gana(move, move_no_aleatorio, repeticiones = 1500)



