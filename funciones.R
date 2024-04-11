# -------------------------------------------
#   Miguel A. Castellanos
# -------------------------------------------

# Fecha:
# Descripcion:
# -------------------------------------------


# -----------------------------------------------------
# Crea una partida

nuevojuego <- function(){
  t <- array(NA,c(4,4,4))
  dimnames(t) <- list(1:4, 1:4, c("ALTURA", "COLOR", "FORMA", "AGUJERO"))
  
  
  nombres <- data.frame(
    altura = rep(c("ALTA", "BAJA"), each = 8),
    color = rep(c("BLANCA", "NEGRA"), 2, each = 4),
    forma = rep(c("REDONDA", "CUADRADA"), 4, each = 2),
    tapa = rep(c("PLANA", "AGUJERO"), 8))
  
  nombres = sprintf("%s_%s_%s_%s", nombres$altura, nombres$color, nombres$forma, nombres$tapa)
  
  ficha <- data.frame(
    altura = rep(c(0, 1), each = 8),
    color = rep(c(0, 1), 2, each = 4),
    forma = rep(c(0, 1), 4, each = 2),
    tapa = rep(c(0, 1), 8),
    descripcion = nombres,
    index = 1:16,
    disponible = 1)
  
  
  return(list(
    estado = 1,
    ficha = ficha,
    tablero = t
  ))
}


diag_inv <- function(r) r[(n<-nrow(r))^2-(1:n)*(n-1)]

# -----------------------------------------------------
# Actualiza el tablero con el movimiento m

update <- function(partida, m){
  if(partida$estado == 0) stop("Esta partida ya ha terminado")
  r <- m[1] # row
  c <- m[2] # col
  f <- m[3] # ficha
  
  # comprobamos que es un movimiento valido --- ESTO TAMBIÉN ESTÁ EN MOVE
  if (!is.na(partida$tablero[r, c, 1])) stop("Posicion ocupada")
  if (partida$ficha[f, "disponible"] == 0) stop("Ficha no disponible")
  
  # marcamos el tablero
  partida$tablero[r, c, ] <- as.numeric(partida$ficha[f, 1:4])
  
  # indicamos que ya no esta disponible la ficha
  partida$ficha[f, "disponible"] <- 0
  
  # comprobamos si el movimiento es ganador buscando en cada una de los rasgos
  # si se ha formado una linea horizontal, vertical o diagonal
  
  win <- 0  # usamos un flag para las comprobaciones
  
  for (i in 1:4){
    if (length(unique(partida$tablero[r, , i])) == 1) win = 1 
    if (length(unique(partida$tablero[ , c, i])) == 1) win = 1 
    if (r == c) if (length(unique(diag(partida$tablero[ , , i]))) == 1) win = 1 
    if ((r + c) == 5) if (length(unique(diag_inv(partida$tablero[ , , i]))) == 1) win = 1 
  }
  
  
  # EMPATES
  if(anyNA(partida$tablero) == FALSE && win == 0) {
    partida$estado <- 2  
    resultado <- "empate"
    }
  
  # JUGADAS GANADORAS
  if (win == 1)  {partida$estado <- 0; 
  resultado <- "victoria" 
  #print("JUGADA GANADORA")
  } 
  return(partida)
}






