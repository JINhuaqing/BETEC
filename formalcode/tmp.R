rm(list=ls())

library(magrittr)
library(parallel)
source("utilities.R")




alpha0 <- 1
beta0 <- 1
nMin <- 1

# Test probabilities
p0 <- 0.3
p1 <- 0.5

# 4 hyper-parameters
pi1 <- 0.6
pi2 <- 0.7

a1 <- 0.05
a2 <- 0.1
b1 <- 0.01
b2 <- 0.01

res1 <- BETEC(p0, p1, pi1, pi2, a1, a2, alpha0=1, beta0=1, nMax=200, N=10000)
res2 <- deltaBETEC(p0, p1, pi1, pi2, b1, b2)
round(Combo.Results(res2, p0, p1), 4)


