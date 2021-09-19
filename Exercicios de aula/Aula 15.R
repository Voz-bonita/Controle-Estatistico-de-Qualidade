totais <- c(64.4, 96.1, 59.7, 161.1)
n <- 4

Acontraste <- (totais[2] + totais[4] - totais[1] - totais[3])
Afx <- Acontraste/(2*n)

Bconstraste <- (totais[3] + totais[4] - totais[1] - totais[2])
Bfx <- Bconstraste/(2*n)

ABconstraste <- (totais[1] + totais[4] - totais[2] - totais[3])
ABfx <- ABconstraste/(2*n)

SQA <- Acontraste^2/(4*n)
SQB <- Bconstraste^2/(4*n)
SQAB <- ABconstraste^2/(4*n)


### Regressão ---------------------
A <- c(-1,-1,1,1)
B <- c(-1,1,-1,1)
AB <- A*B

23.83125 + (Afx*A + Bfx*B + ABfx*AB)/2