pacman::p_load(dplyr, tibble, ggplot2, reshape2)

norm_entre <- function(mu, mean, sd, n, qnt_sd = 3) {
  # Retorna probabilidade de estar entre certos limites de controle
  lic <- mu - qnt_sd*sd/sqrt(n)
  lsc <- mu + qnt_sd*sd/sqrt(n)
  prob <- pnorm(lsc, mean=mean, sd=sd/sqrt(n)) - pnorm(lic, mean=mean, sd=sd/sqrt(n))
  return(prob)
}

media  <- 74
sd <- 0.01
n <- seq(5,15,5)

medias <- seq(74, 74.025, 0.0005)


n5 <- norm_entre(mu = 74, mean = medias, sd = sd, n = n[1])
n10 <- norm_entre(mu = 74, mean = medias, sd = sd, n = n[2])
n15 <- norm_entre(mu = 74, mean = medias, sd = sd, n = n[3])

ceq <- tibble("Medias" = medias,
              "n = 5" = n5,
              "n = 10" = n10,
              "n = 15" = n15) %>%
  melt(id.vars = "Medias") %>%
  rename_all(~c("Medias", "n", "Probabilidade"))

ggplot(data = ceq) +
  geom_line(aes(x = `Medias`, y = `Probabilidade`, color = `n`), size = 2)



##### Exercício TMA

t <- 1 # Hora

### Caso 1
p <- 1 - norm_entre(mu = 74, mean = 74.01, sd = sd, n = n[2])
cms <-  1/p
tma <- cms*t

h <- trunc(tma)
min <- round((tma %% 1)*60, 0)
paste0(h,"h",min,"min")

### Caso 2
p <- 1 - norm_entre(mu = 74, mean = 74.02, sd = sd, n = n[3])
cms <-  1/p
tma <- cms*t

h <- trunc(tma)
min <- round((tma %% 1)*60, 0)
paste0(h,"h",min,"min")


##### Exercício gráfico de controle (Probabilidades)

### 2 fora em 3 pontos consecutivos, para entre 2 e 3 desvios-padrão
fora <- norm_entre(mu = 74, mean = 74, sd = sd, n = 3, qnt_sd = 3) -
  norm_entre(mu = 74, mean = 74, sd = sd, n = 3, qnt_sd = 2)

p <- dbinom(2, 3, fora)


### 4 fora em 5 pontos consecutivos, para 1 desvio-padrão
fora <- norm_entre(mu = 74, mean = 74, sd = sd, n = 5, qnt_sd = 3) -
  norm_entre(mu = 74, mean = 74, sd = sd, n = 5, qnt_sd = 1)

p <- dbinom(2, 3, fora)


### 8 pontos consecutivos de um mesmo lado da linha central
"Como a distribuicao e simetrica,
as probabilidades a esquerda e a direita sao iguais"

lic <- media - 3*sd/sqrt(8)
esquerda <- 0.5 - pnorm(lic, mean = media, sd = sd/sqrt(8))
p <- esquerda^8
















