pacman::p_load(dplyr)

#### CUSUM
m0 <- 10
n <- 1
sd <- 1
delta <- 1

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
    Ce[i+1] <- (X[i] - (m0 + K) + Ce[i]) %>%
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

}
