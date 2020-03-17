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
pi1 <- 0.8
pi2 <- 0.9

a1 <- 0.1 
a2 <- 0.25
b1 <- 0.05
b2 <- 0.05

#res1 <- BETEC(p0, p1, pi1, pi2, a1, a2, alpha0=1, beta0=1, nMax=200, N=10000)
res2 <- deltaBETEC(p0, p1, pi1, pi2, b1, b2)
res2
