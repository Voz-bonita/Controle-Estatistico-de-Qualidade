T_Hotelling <- function (n, p, m, alpha = 0.001) {
  denom <- (m*n - m - p + 1)
  LSC_1 <- p * (m - 1) * (n - 1) / denom * qf(1-alpha, p, denom)
  LSC_2 <- p * (m + 1) * (n - 1) / denom * qf(1-alpha, p, denom)
  LSE <- qchisq(1-alpha, p)

  return (c("Fase1" = LSC_1, "Fase2" = LSC_2, "LSC-Chisq" = LSE))
}


T_Hotelling(n = 10, p = 2, m = 20, alpha = 0.001)
T_Hotelling(n = 3, p = 15, m = 20, alpha = 0.01)
T_Hotelling(n = 3, p = 5, m = 30, alpha = 0.01)


T_Hotelling_n1 <- function (p, m, alpha = 0.001) {
  if (m <= 100) { LSC <- p * (m + 1) * (m - 1) / (m^2 - m*p) * qf(1-alpha, p, m-p) }
  else { LSC <- p * (m - 1) / (m - p) * qf(1-alpha, p, m-p) }
  LSE <- qchisq(1-alpha, p)

  return (c("Fase2" = LSC, "LSC-Chisq" = LSE))
}



