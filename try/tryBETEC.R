rm(list=ls())
library(magrittr)
library(parallel)

source("utilities.R")





alpha0 <- 1
beta0 <- 1
nMin <- 1

p01s <- list(
             #c(0.05, 0.25), 
             #c(0.1, 0.3),
             #c(0.2, 0.4), 
             #c(0.3, 0.5),
             #c(0.4, 0.6),
             #c(0.5, 0.7)
             c(0.6, 0.8),
             c(0.7, 0.9)
             )
a12s <- list(
             c(0.01, 0.07),
             c(0.01, 0.08),
             c(0.01, 0.1),
             c(0.02, 0.07),
             c(0.02, 0.08),
             c(0.02, 0.1)
             )
pi1 <- 0.50
pi2 <- 0.55



ress <- list()

for (i in 1:length(p01s)){
    p01 <- p01s[[i]]
    for (j in 1:length(a12s)){
        a12 <- a12s[[j]]
        p0 <- p01[1]
        p1 <- p01[2]
        a1 <- a12[1]
        a2 <- a12[2]
        idx <- (i-1)*length(a12s) + j
        res <- BETEC(p0, p1, pi1, pi2, a1, a2, alpha0=1, beta0=1, nMax=200, N=100000)
        ress[[idx]] <- list(a12=a12, p01=p01, res=res)
    }
}
#round(Combo.Results(res1, p0, p1), 4)
save(ress, file="BETECp4.Rdata")
