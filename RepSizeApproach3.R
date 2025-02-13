repApp3 <- function(alpha, gamma, t, sigma)
{ r <- 1
power <- 0
nu1 <- t-1
#Input the values of the treatment means
u <- c(20, 18, 16)
mu <- mean(u)
while(power < gamma) {
  r <- r+1
  nu2 <- t*(r-1)
  L <- r*(sum((u-mu)^2))/sigma^2
  Fcr <- qf(1-alpha, nu1, nu2)
  power <- 1-pf(Fcr, nu1, nu2,L)
}
print(cbind(r, nu1, nu2, Fcr, L, power)) }

repApp3(.01,.9, 3, sqrt(12))

repApp4 <- function(alpha, gamma, t, D, sigma)
{ r <- 1
power <- 0
nu1 <- t-1
u <- c(20, 18, 16)
mu <- mean(u)
while(power < gamma) {
  r <- r+1
  nu2 <- t*(r-1)
  L <- r*t*D^2/sigma^2
  Phi <- sqrt(L/t)
  Fcr <- qf(1-alpha, nu1, nu2)
  power <- 1-pf(Fcr, nu1, nu2,L)
}
print(cbind(D,t,r, nu2, L, Fcr, Phi, power)) }

repApp4(.01, .9, 3, 4, sqrt(12))

