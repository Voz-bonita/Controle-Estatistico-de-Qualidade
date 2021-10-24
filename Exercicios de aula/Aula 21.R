pacman::p_load("dplyr")

resultados <- list(
  "Exp1" = c(84.5, 84.2, 84.9, 84.5, 84.3),
  "Exp2" = c(84.9, 84.6, 85.9, 83.5, 84.0),
  "Exp3" = c(85.0, 84.0, 86.6, 84.9, 85.2)
)

fkn <- data.frame(matrix(ncol = 10, nrow = 3)) %>%
  rename_all( ~ c('k', seq(2,10)))
fkn[1,] <- c(5,     0.30, 	0.35,	0.37,	0.38,	0.39,	0.40,	0.40,	0.40,	0.41)
fkn[2,] <- c(9,	    0.24,	0.27,	0.29,	0.30,	0.31,	0.31,	0.31,	0.32,	0.32)
fkn[3,] <- c(10,	0.23,	0.26,	0.28,	0.29,	0.30,	0.30,	0.30,	0.31,	0.31)

EVOP <- function (resultados.lista, fkn) {
  init_ans <- function () {
    ans <- data.frame(matrix(ncol = 8, nrow = 13))
    condicoes <- c("", "(i)", "(ii)", "(iii)", "(iv)", "(v)", "(vi)")
    dp_calc <- c("n=", "Soma Previa S =", "Media Previa S=", "Nova S", "RD", "Soma Nova S", "Nova Media S")
    efeito <- c("Efeitos", "A", "B", "AB", "Mudanca na Media")
    limites <- c("Limites do Erro", "Media 1,96S/n^(1/2)", "Efeitos 1,96S/n^(1/2)", "", "Mud Media 1,75S/n^(1/2)")
    ans$X1 <- c(condicoes, "", efeito)
    ans$X7 <- c(dp_calc, "", limites)
    ans[1, 2:6] <- paste0("(", seq(1, 5), ")")
    return(ans)
  }

  fill_ans <- function (ans, Componentes, DP_info, Efeitos, Limites) {
    ans[1,8] <- DP_info[1]

    for (i in 1:6) {
      ans[i+1, 2:6] <- Componentes[[i]]
      ans[i+1, 8] <- DP_info[[i+1]]
    }
    for (i in 0:3) {
      ans[10+i, 2] <- Efeitos[[i+1]]
      ans[10+i, 8] <- Limites[[i+1]]
    }
    return(ans)
  }

  n <- 0
  # Componentes do experimento "zero"
  # Gambiarra computacional para fazer tudo em um loop
  Ci <- rep(0, 5)
  Cii <- rep(0, 5)
  Ciii <- rep(0, 5)
  Civ <- rep(0, 5)
  Cv <- rep(0, 5)
  Cvi <- rep(0,5)
  NovaS <- 0
  MediaS <- 0
  SumNovaS <- 0
  NovaMediaS <- 0

  base <- data.frame(matrix(ncol = 8, nrow = 0))
  signs <- list("A" = c(-1,1,1,-1),
                "B" = c(-1,1,-1,1),
                "AB" = c(1,1,-1,-1))
  for (experimento in resultados.lista) {
    n <- n + 1



    # Componentes
    Ci <- Cv
    Cii <- Cvi
    Ciii <- experimento
    Civ <- Cii - Ciii
    if (n == 1) {Civ <- 0}
    Cv <- Ci + Ciii
    Cvi <- Cv/n
    Componentes <- list(Ci, Cii, Ciii, Civ, Cv, Cvi)

    Afx <- sum(Cvi[-1] * signs[["A"]]) / (2*n)
    Bfx <- sum(Cvi[-1] * signs[["B"]]) / (2*n)
    ABfx <- sum(Cvi[-1] * signs[["AB"]]) / (2*n)
    Mud_media <- (sum(Cv[-1]) - 4*Cv[1]) / 5
    Efeitos <- list(Afx, Bfx, ABfx, Mud_media)


    SumPreviaS <- SumNovaS
    MediaPreviaS <- NovaMediaS
    Amplitude <- sum(abs(range(Civ)))
    S <- Amplitude * filter(fkn, k == 5)[[n]]
    SumNovaS <- SumPreviaS + S

    if (n == 1) {
      NovaMediaS <- SumNovaS
    } else {
      NovaMediaS <- SumNovaS / (n-1)
    }

    DP_calc <- list(n, SumPreviaS, MediaPreviaS, S, Amplitude, SumNovaS, NovaMediaS)

    Limites <- c(1.96 * S / sqrt(n),
                 1.96 * S / sqrt(n),
                 1.75 * S / sqrt(n))
    Limites <- list(Limites[1], Limites[2], "", Limites[3])

    ans <- init_ans()
    ans <- fill_ans(ans = ans, Componentes = Componentes,
                    Efeitos = Efeitos, DP_info = DP_calc,
                    Limites = Limites)

    base <- rbind(base, ans)

  }
  return(base)
}

planilha <- EVOP(resultados.lista = resultados, fkn = fkn)