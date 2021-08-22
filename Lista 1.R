pacman::p_load(dplyr)
#### Questao 1

n <- 8
media <- 2000/50
R_medio <- 250/50
A2 <- 0.373

### a)
GC_media <- c(media + A2*R_medio, media, media - A2*R_medio)
GC_R <- c(R_medio*0.076, R_medio, R_medio*1.924)

### b)
sdhat <- R_medio/2.704
media + 3*sdhat
media - 3*sdhat

### c)

inferior <- 41 - 5
superior <- 41 + 5
##  1.6 pecas a cada 100 fica fora das especificacoes
p1 <- pnorm(inferior, mean = media, sd = sdhat) + pnorm(superior, mean = media, sd = sdhat, lower.tail = F)

Cp <- (superior - inferior) / (6*sdhat)
1/Cp
## O processo usa cerca de 111% da faixa de especificacao
## O processo esta com media desregulada e variancia fora de controle?

### d)
pnorm(GC_media[3], mean = media, sd = sdhat) +
  pnorm(GC_media[1], mean = media, sd = sdhat, lower.tail = F)

#### Questao 2

sd_m <- 1.5
media <- 20

### a)
sd_hat <- sd_m/0.940

### b)
GC_media <- c(media + 1.427*sd_m, media, media - 1.427*sd_m)
GC_sd <- c(sd_hat*0, sd_hat, sd_hat*2.089)

### c)
pnorm(GC_media[1], mean = 22, sd = sd_m) -
  pnorm(GC_media[3], mean = 22, sd = sd_m)


#### Questao 3
p <- 0.1
p_new <- 0.2
delta <- p_new - p
L <- 3
n <- (L/delta)^2 * p*(1-p)


#### Questao 4
### a)
p <- 0.07
n <- 400
LIC <- p - 3*sqrt(p*(1-p)/n)
LSC <- p + 3*sqrt(p*(1-p)/n)

### b)
p_new <- 0.1
1 - (pbinom(LSC*n, n, p_new) - pbinom(LIC*n, n, p_new))


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