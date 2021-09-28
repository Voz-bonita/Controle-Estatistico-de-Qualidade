pacman::p_load("purrr", "dplyr")

n <- 2
k <- 3

sign_vector <- c(-1, -1, -1, 1,	1,	1,	-1,
                1,	-1,	-1,	-1,	-1,	1,	1,
                -1,	1,	-1,	-1,	1,	-1,	1,
                1,	1,	-1,	1,	-1,	-1,	-1,
                -1,	-1,	1,	1,	-1,	-1,	1,
                1,	-1,	1,	-1,	1,	-1,	-1,
                -1,	1,	1,	-1,	-1,	1,	-1,
                1,	1,	1,	1,	1,	1,	1,
                -1,	-1,	-1,	1,	1,	1,	-1,
                1,	-1,	-1,	-1,	-1,	1,	1,
                -1,	1,	-1,	-1,	1,	-1,	1,
                1,	1,	-1,	1,	-1,	-1,	-1,
                -1,	-1,	1,	1,	-1,	-1,	1,
                1,	-1,	1,	-1,	1,	-1,	-1,
                -1,	1,	1,	-1,	-1,	1,	-1,
                1,	1,	1,	1,	1,	1,	1)

# Forma de tabela para facilitar os calculos
matrix_signs <- as_tibble(t(matrix(sign_vector, nrow = 7, ncol = 16)))

exp1 <- c(9,10,9,12,11,10,10,16)
exp2 <- c(7,12,11,15,10,13,8,14)
experimento <- c(exp1, exp2)

Constrastes <- map_dbl(matrix_signs, ~sum(.x * experimento))
names(Constrastes) <- c("A", "B", "C", "AB", "AC", "BC", "ABC")

Efeitos <- Constrastes / (2^(k-1) * n)

SQs <- Constrastes^2 / (n * 2^k)
SQmodelo <- sum(SQs)

mu_hat <- mean(experimento)

SQtotal <- sum((experimento - mu_hat)^2)
SQres <- SQtotal - SQmodelo

Coeficientes <- Efeitos / 2

