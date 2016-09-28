# This script performs a MonteCarlo simulation to
# test the CI intervals for the estimation of p
# in a Bernoulli proces


rm(list = ls())

p <- 0.8
N <- 1000
n_mc <- 1000

ci <- 0.9
  
set.seed(3)  

seeds <- round(10000*runif(n_mc,0,1))
pest_v <- vector("numeric",n_mc)
ci_ok <- vector("numeric",n_mc)
ci_low <- vector("numeric",n_mc)
ci_high <- vector("numeric",n_mc)

for(ix_mc in 1:n_mc) {
  
  set.seed(seeds[ix_mc])
  
  xc <- runif(N)
  x <- xc<p
  
  pest <-  sum(x)/N
  
  pest_v[ix_mc] <- pest
  
  a <- 1-ci
  
  ci_low[ix_mc] <- pest - qnorm(1-a/2)*sqrt(pest*(1-pest)/N)
  ci_high[ix_mc] <- pest - qnorm(a-a/2)*sqrt(pest*(1-pest)/N)
  
  if((ci_low[ix_mc] <= p) & (p <= ci_high[ix_mc])) {
    ci_ok[ix_mc] <- 1  
  } else {
    ci_ok[ix_mc] <- 0
  }
  
}

plot(pest_v,col="red")
lines(ci_low,col="blue")
lines(ci_high,col="blue")

hist(pest_v,freq = FALSE)

plot(ci_ok)

hist(ci_ok,freq = FALSE)

