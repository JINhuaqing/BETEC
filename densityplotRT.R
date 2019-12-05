rm(list=ls())
library(magrittr)
source("utilities.R")

# compute P(p|R)
post.density.R <- function(p, r1, r, n1, n, alpha0, beta0){
    dbeta(p, alpha0, beta0)*rej.prob(r1, r, n1, n, p)
}

PR.f <- function(r1, n1, r, n, alpha0=1, beta0=1, N=200000){
    ps <- rbeta(N, alpha0, beta0)
    probs <- sapply(ps, rej.prob, r1=r1, r=r, n1=n1, n=n)
    PR <- mean(probs)
    PR
}

post.f.R <- function(r1, n1, r, n){
  ff <- function(x){post.density.R(x, r1, r, n1, n, 1, 1)}
  ff
}

# bBET
f1 <- post.f.R(1, 10, 26, 84)
f2 <- post.f.R(1, 6, 27, 73)
f3 <- post.f.R(2, 6, 24, 49)
f4 <- post.f.R(3, 6, 17, 27)
f1s <- sapply(seq(0, 1, 0.01), f1)
f2s <- sapply(seq(0, 1, 0.01), f2)
f3s <- sapply(seq(0, 1, 0.01), f3)
f4s <- sapply(seq(0, 1, 0.01), f4)
f1s <- f1s / PR.f(1, 10, 26, 84)
f2s <- f2s / PR.f(1, 6, 27, 73)
f3s <- f3s / PR.f(2, 6, 24, 49)
f4s <- f4s / PR.f(3, 6, 17, 27)
jpeg(file="rbBET.jpg")
plot(seq(0, 1, 0.01), f1s, type="l", lty=1, col="red", lwd=2, xlab="p", ylab="Density", ylim=c(0, 3.5), xaxt="n")
lines(seq(0, 1, 0.01), f2s, type="l", lty=2, col="blue", lwd=2)
lines(seq(0, 1, 0.01), f3s, type="l", lty=3, col="green", lwd=2)
lines(seq(0, 1, 0.01), f4s, type="l", lty=4, col="dark red", lwd=2)

ex1 <- expression(paste(p[0]==0.05, ", ", p[1]==0.25))
ex2 <- expression(paste(p[0]==0.1, ", ", p[1]==0.3))
ex3 <- expression(paste(p[0]==0.2, ", ", p[1]==0.4))
ex4 <- expression(paste(p[0]==0.3, ", ", p[1]==0.5))
axis(1, seq(0, 1, 0.05), seq(0, 1, 0.05))
text(0, 3.5, labels="(C)")
legend("topright", c(ex1, ex2, ex3, ex4), lty=1:4, col=c("red", "blue", "green", "dark red"))
dev.off()



# delta-bBET
f1 <- post.f.R(1, 10, 14, 41)
f2 <- post.f.R(2, 13, 16, 41)
f3 <- post.f.R(6, 23, 22, 44)
f4 <- post.f.R(10, 27, 26, 43)
f1s <- sapply(seq(0, 1, 0.01), f1)
f2s <- sapply(seq(0, 1, 0.01), f2)
f3s <- sapply(seq(0, 1, 0.01), f3)
f4s <- sapply(seq(0, 1, 0.01), f4)
f1s <- f1s / PR.f(1, 10, 14, 41)
f2s <- f2s / PR.f(2, 13, 16, 41)
f3s <- f3s / PR.f(6, 23, 22, 44)
f4s <- f4s / PR.f(10, 27, 26, 43)
jpeg(file="rdelta-bBET.jpg")
plot(seq(0, 1, 0.01), f1s, type="l", lty=1, col="red", lwd=2, xlab="p", ylab="Density", ylim=c(0, 3.5), xaxt="n")
lines(seq(0, 1, 0.01), f2s, type="l", lty=2, col="blue", lwd=2)
lines(seq(0, 1, 0.01), f3s, type="l", lty=3, col="green", lwd=2)
lines(seq(0, 1, 0.01), f4s, type="l", lty=4, col="dark red", lwd=2)

ex1 <- expression(paste(p[0]==0.05, ", ", p[1]==0.25))
ex2 <- expression(paste(p[0]==0.1, ", ", p[1]==0.3))
ex3 <- expression(paste(p[0]==0.2, ", ", p[1]==0.4))
ex4 <- expression(paste(p[0]==0.3, ", ", p[1]==0.5))
axis(1, seq(0, 1, 0.05), seq(0, 1, 0.05))
text(0, 3.5, labels="(D)")
legend("topright", c(ex1, ex2, ex3, ex4), lty=1:4, col=c("red", "blue", "green", "dark red"))
dev.off()