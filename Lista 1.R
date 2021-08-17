pacman::p_load(dplyr)
#### Questao 5

relu <- function (xi) {
  ans <- max(0, xi)
  return(ans)
}

CUSUM <- function(X, m0,delta,sd) {
  X <- as.vector(X)

  K <- delta*sd/2
  H <- 5*sd
  m1 <- m0 + delta*sd

  N <- length(X)
  Cd <- numeric(N+1) # C+
  Ce <- numeric(N+1) # C-
  Nd <- numeric(N+1)
  Ne <- numeric(N+1)


  for (i in seq(1,N,1)) {
    Cd[i+1] <- (X[i] - (m0 + K) + Cd[i]) %>%
      relu()
    Ce[i+1] <- ((m0 - K) - X[i] + Ce[i]) %>%
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
    "Cd" = Cd[2:(N+1)],
    "Ce" = Ce[2:(N+1)],
    "Nd" = Nd[2:(N+1)],
    "Ne" = Ne[2:(N+1)]
  )

  return(obs)

}

sd <- 0.05
H <- 4.77*sd
K <- sd/2
delta <- 1
m0 <- 8
X <- c(8.00, 8.01, 8.02, 8.01, 8.00, 8.01,
       8.06, 8.07, 8.01, 8.04, 8.02, 8.01,
       8.05, 8.04, 8.03, 8.05, 8.06, 8.04,
       8.05, 8.06, 8.04, 8.02, 8.03, 8.05)

CUSUM(X, m0, delta, sd)
m0 + K + 0.240/14