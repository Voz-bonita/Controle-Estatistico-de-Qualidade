#### Grafico de controle da fracao de nao conformes
x_sum <- 347
m <- 30
n <- 50

LC <- x_sum/m/n
LIC <- LC - 3*sqrt(LC*(1-LC)/n)
LSC <- LC + 3*sqrt(LC*(1-LC)/n)

