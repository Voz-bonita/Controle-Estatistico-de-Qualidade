### Fracao Nao-conforme
tamanho.n <- function(L,p,ph){
  n <- L^2 / (ph-p)^2 * p*(1-p)
  return(n)
}

## Exemplos
tamanho.n(3,0.01,0.05)
tamanho.n(3,0.01,0.03)

### LIC positivo
p <- 0.05
L <- 3
(1-p)/p*L^2

### CO
LIC <- 0.0303
LSC <- 0.3697
p <- 0.2
n <- 50
X <- n*p
alpha <- 1-(pbinom(LSC*n, n, p) - pbinom(LIC*n, n, p))

## Exercicio
LIC <- 0.0303
LSC <- 0.3697
p <- 0.25
n <- 50
beta <- pbinom(LSC*n, n, p) - pbinom(LIC*n, n, p)
alfa <- 1 - beta

CMS0 <- 1/alfa
CMS1 <- 1/beta

