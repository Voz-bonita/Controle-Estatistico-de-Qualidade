pacman::p_load(dplyr)

### Inspecao de geladeiras
relu <- function(LIC){
  LIC_n <- max(0,LIC)
  return(LIC_n)
}
## (i)
c <- 16/30
LIC <- (c - 3*sqrt(c)) %>%
  relu()
LSC <- c + 3*sqrt(c)

## (ii)
alpha <- ppois(LSC, c, lower.tail = F)

## (iii)
beta <- ppois(LSC, 2)

## (iv)
CMS <- 1/(1-beta)

### Nao conformidades por unidade
## (i)
n <- 100
p <- 0.08
np <- n*p
LIC <- (np - 3*sqrt(p*(1-p)/n)*n) %>%
  relu()
LSC <- np + 3*sqrt(p*(1-p)/n)*n

## (ii)
pbinom(LSC, n, p,lower.tail = F)
ppois(LSC, np, lower.tail = F)

## (iii)
p <- 0.2
mu <- n*p
sd <- sqrt(n*p*(1-p))
pbinom(LSC, n, p)
beta <- pnorm(LSC + 0.5, mean = mu, sd = sd)

## (iv)
(1-beta) + beta*(1-beta)

### Fracao de nao conforme
## (i)
p <- 0.01
4*p*(1-p)/p^2 # < n

## (ii)
(2/(0.04-p))^2*p*(1-p)
