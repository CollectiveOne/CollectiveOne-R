rm(list = ls())

p<-0.8
xc <- runif(100)
x <- xc<p

plot(xc)
grid()

plot(x)
grid()