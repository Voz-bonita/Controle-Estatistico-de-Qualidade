#### Media Movel
media_movel <- function(X, m0, sd, w){
  X <- as.vector(X)

  n <- length(X)
  W <- c(seq(1,w), rep(w,n-w))

  LSC <- numeric(n)
  LIC <- numeric(n)
  M <- numeric(n)

  for (i in 1:n) {
    LIC[i] <- m0 - 3*sd/sqrt(W[i])
    LSC[i] <- m0 + 3*sd/sqrt(W[i])
    start <- max(0, i-4)
    M[i] <- mean(X[start:i])
  }

  out <- data.frame(
    "LIC" = LIC,
    "Media" = M,
    "LSC" = LSC
  )

  return(out)
}



m0 <- 10
sd <- 1
w <- 5
X <- c(9.45,7.99,9.29,11.66,12.16,10.18,8.04,11.46,9.20,10.34,9.03,11.47,10.51,9.40,
       10.08,9.37,10.62,10.31,8.52,10.84,10.90,9.33,12.29,11.50,10.60,11.08,10.38,11.62,11.31,10.52)

media_movel(X, m0, sd, w)

#### MMEP
mmep <- function(X, m0, sd, L, lambda){
  X <- as.vector(X)

  n <- length(X)
  Z <- numeric(n)
  Z[1] <- lambda*X[1] + (1-lambda)*m0
  for (i in 2:n) {
    Z[i] <- lambda*X[i] + (1-lambda)*Z[i-1]
  }


  LSC <- numeric(n)
  LIC <- numeric(n)

  for (i in 1:n) {
    LIC[i] <- m0 - L*sd*sqrt(lambda/(2-lambda)*(1-(1-lambda)^(2*i)))
    LSC[i] <- m0 + L*sd*sqrt(lambda/(2-lambda)*(1-(1-lambda)^(2*i)))
  }

  out <- data.frame(
    "LIC" = LIC,
    "Media" = Z,
    "LSC" = LSC
  )

  return(out)
}

m0 <- 10
sd <- 1
L <- 2.7
lambda <- 0.1

X <- c(9.45, 7.99, 9.29, 11.66, 12.16, 10.18, 8.04, 11.46, 9.20, 10.34, 9.03,
  11.47, 10.51, 9.40, 10.08, 9.37, 10.62, 10.31, 8.52, 10.84, 10.90, 9.33,
  12.29, 11.50, 10.60, 11.08, 10.38, 11.62)

mmep(X, m0, sd, L, lambda)
mmep(X, 15, sd, L, lambda)