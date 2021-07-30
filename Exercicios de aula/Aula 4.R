##### Aneis de pistao
n <- 25 # Amostras de tamanho 5

media_s <- 1850.028
media_m <- media_s / n

sd_s <- 0.2350
sd_m <- sd_s / n

## Media
LIC <- media_m - 0.577*sd_m
LSC <- media_m + 0.577*sd_m

## SD
LIC <- sd_m*0
LSC <- sd_m*2.089


##### Graficos de controle (Geral)
### 1)
media <- 700
sd <- 7.979/0.921

### 2)
# Probabilidade de nao conformes
1-(pnorm(705+15, mean = media, sd = sd) - pnorm(705-15, mean = media, sd = sd))

### 3)
# Limites pre-estabelecidos
1-(pnorm(710, mean = media, sd = sd/sqrt(4)) - pnorm(690, mean = media, sd = sd/sqrt(4)))

