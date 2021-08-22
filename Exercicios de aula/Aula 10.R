pacman::p_load(dplyr)

#### Ex1
X <- c(102,97,104,93,100,105,96,98,105,99)

relu <- function (xi) {
  ans <- max(0, xi)
  return(ans)
}

CUSUM <- function(X, m0, sd, k, h, headstart = 0) {
  X <- as.vector(X)

  K <- k*sd
  H <- h*sd

  N <- length(X)
  Cd <- numeric(N+1) # C+
  Ce <- numeric(N+1) # C-
  Nd <- numeric(N+1)
  Ne <- numeric(N+1)
  Xd <- numeric(N)
  Xe <- numeric(N)


  Cd[1] <- H*headstart
  Ce[2] <- H*headstart


  for (i in seq(1,N,1)) {
    Xd[i] <- X[i] - K - m0
    Xe[i] <- (m0-K) - X[i]
    Cd[i+1] <- (Xd[i] + Cd[i]) %>%
      relu()
    Ce[i+1] <- (Xe[i] + Ce[i]) %>%
      relu()

    if (Cd[i+1]) {
      Nd[i+1] <- Nd[i] + 1
    } else {
      Nd[i+1] <- 0
    }

    if (Ce[i+1]) {
      Ne[i+1] <- Ne[i] + 1
    } else {
      Ne[i+1] <- 0
    }
  }

  obs <- data.frame(
    "Xi" = X,
    "Xd" = Xd,
    "Xe" = Xe,
    "Cd" = Cd[2:(N+1)],
    "Ce" = Ce[2:(N+1)],
    "Nd" = Nd[2:(N+1)],
    "Ne" = Ne[2:(N+1)]
  )

  return(obs)

}

m0 <- 100
CUSUM(X, m0, 1, 3, 12, headstart = 0.5)
CUSUM(X+5, m0, 1, 3, 12, headstart = 0.5)

#### Ex2
CUSUM_padronizado <- function(X, m0, sd, k, h, headstart = 0) {
  X <- as.vector(X)

  K <- k*sd
  H <- h*sd

  N <- length(X)
  Cd <- numeric(N+1) # C+
  Ce <- numeric(N+1) # C-
  Nd <- numeric(N+1)
  Ne <- numeric(N+1)
  Xd <- numeric(N)
  Xe <- numeric(N)


  Cd[1] <- H*headstart
  Ce[2] <- H*headstart


  for (i in seq(1,N,1)) {
    Xd[i] <- X[i] - k
    Xe[i] <- -k - X[i]
    Cd[i+1] <- (Xd[i] + Cd[i]) %>%
      relu()
    Ce[i+1] <- (Xe[i] + Ce[i]) %>%
      relu()

    if (Cd[i+1]) {
      Nd[i+1] <- Nd[i] + 1
    } else {
      Nd[i+1] <- 0
    }

    if (Ce[i+1]) {
      Ne[i+1] <- Ne[i] + 1
    } else {
      Ne[i+1] <- 0
    }
  }

  obs <- data.frame(
    "Xi" = X,
    "Xd" = Xd,
    "Xe" = Xe,
    "Cd" = Cd[2:(N+1)],
    "Ce" = Ce[2:(N+1)],
    "Nd" = Nd[2:(N+1)],
    "Ne" = Ne[2:(N+1)]
  )

  return(obs)

}

m0 <- 50
sd <- 10
X <- c(50,40,60,50,70,80,100,120)
Y <- (X-m0)/sd

CUSUM_padronizado(Y, m0, sd, 0.5, 5)

#### Ex3
V <- (sqrt(abs(Y)) - 0.822) / (0.349)
CUSUM_padronizado(V, 0, sd, 0.5, 5)