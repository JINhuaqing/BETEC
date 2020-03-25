rm(list=ls())

setwd("C:/Users/Dell/Documents/ProjectCode/BETEC/formalcode")
library(magrittr)
library(parallel)
source("utilities.R")




alpha0 <- 1
beta0 <- 1
nMin <- 1

# Test probabilities
p0 <- 0.05
p1 <- 0.25

# 4 hyper-parameters
pi1 <- 0.50
pi2 <- 0.55

a1 <- 0.10
a2 <- 0.15
b1 <- 0.030
b2 <- 0.058
# (p0, p1) = (0.05,0.25), (b1, b2)= (0.03, 0.58)
# (p0, p1) = (0.1,0.3), (b1, b2)= (0.03, 0.60)
# (p0, p1) = (0.2,0.4), (b1, b2)= (0.03, 0.70)
# (p0, p1) = (0.2,0.4), (b1, b2)= (0.03, 0.70)

res1 <- BETEC(p0, p1, pi1, pi2, a1, a2, alpha0=1, beta0=1, nMax=200, N=10000);res1
round(Combo.Results(res1, p0, p1), 4)

#res2 <- deltaBETEC(p0, p1, pi1, pi2, b1, b2, dlt=0.1);res2
#round(Combo.Results(res2, p0, p1), 4)


