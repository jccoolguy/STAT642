repApp5 <- function(alpha, gamma, t, D, sigma)
{   r     <- 1
    power <- 0
    nu1   <- t-1
    
    while(power < gamma) {
        r      <- r+1
        nu2    <- t*(r-1)
         L     <- r*D^2/(2*sigma^2)
        Phi    <- sqrt(L/t)
        Fcr    <- qf(1-alpha, nu1, nu2)
        power  <- 1-pf(Fcr, nu1, nu2,L)
    }
    print(cbind(D,t,r, nu2, L, Fcr, Phi, power)) }

#repApp5(.05,.90,5,30,sqrt(150))

repApp5(.01, .9, 3, 4, sqrt(12))
