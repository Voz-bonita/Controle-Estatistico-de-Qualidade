#### Grafico de controle da fracao de nao conformes
x_sum <- 347
m <- 30
n <- 50

LC <- x_sum/m/n
LIC <- LC - 3*sqrt(LC*(1-LC)/n)
LSC <- LC + 3*sqrt(LC*(1-LC)/n)

### Hipotese de melhoria

LC.old <- 0.2150
LC.new <- 0.1108

n1 <- 28*50
n2 <- 24*50
phat <- (LC.old*n1 + n2*LC.new)/(n1+n2)

Z0 <- (LC.old-LC.new)/sqrt(phat*(1-phat)*(1/n1 + 1/n2))
Z0 > qnorm(0.95) ## Melhoria aceita
