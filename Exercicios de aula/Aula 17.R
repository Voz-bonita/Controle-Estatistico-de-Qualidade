pacman::p_load("purrr", "dplyr")

exp_fatorial <- function (k, n, experimento, signs_vector, names='') {
  matrix_signs <- as_tibble(t(matrix(signs_vector, nrow = 2^k - 1, ncol = n * 2^k)))
  Constrastes <- map_dbl(matrix_signs, ~sum(.x * experimento))
  if (length(names) > 0) {
    names(Constrastes) <- names
  }

  Efeitos <- Constrastes / (2^(k-1) * n)

  SQs <- Constrastes^2 / (n * 2^k)
  SQmodelo <- sum(SQs)

  mu_hat <- mean(experimento)

  SQtotal <- sum((experimento - mu_hat)^2)
  SQres <- SQtotal - SQmodelo

  Coeficientes <- Efeitos / 2

  BASE <- tibble(
    "Contrastes" = Constrastes,
    "Efeitos" = Efeitos,
    "SQs" = Constrastes,
    "Coeficientes" = Coeficientes
  )

  SQ <- c("SQmodelo" = SQmodelo,
          "SQres" = SQres,
          "SQtotal" = SQtotal)

  return (list("Base" = BASE, "SQ" = SQ))
}

k <- 4
n <- 1

signs <- c(-1,	-1,	1,
           1,	-1,	-1,
           -1,	-1,	1,
           1,	-1,	-1,
           -1,	-1,	1,
           1,	-1,	-1,
           -1,	-1,	1,
           1,	-1,	-1,
           -1,	1,	-1,
           1,	1,	1,
           -1,	1,	-1,
           1,	1,	1,
           -1,	1,	-1,
           1,	1,	1,
           -1,	1,	-1,
           1,	1,	1)




resultados <- c(550,
                669,
                604,
                650,
                633,
                642,
                601,
                635,
                1037,
                749,
                1052,
                868,
                1075,
                860,
                1063,
                729)


analise <- exp_fatorial(n = n, k = k,
                        experimento = resultados,
                        signs_vector = signs,
                        names = c("A", "D", "AD"))
analise$Base
analise$SQ



