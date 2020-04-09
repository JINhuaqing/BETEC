library(clinfun)
ep1 <- 0.10
ep2 <- 0.10

alpha0 <- 1
beta0 <- 1
nMin <- 1

# Test probabilities
p0 <- 0.3
p1 <- 0.5

# 4 hyper-parameters
pi1 <- 0.70
pi2 <- 0.80

a1 <- 0.05
a2 <- 0.15
b1 <- 0.05
b2 <- 0.15

ph2simon(p1, p1+0.2, ep1, ep2, nmax=500)

res1 <- BETEC(p0, p1, pi1, pi2, a1, a2, alpha0=1, beta0=1, nMax=200, N=10000);res1

res2 <- deltaBETEC(p0, p1, pi1, pi2, b1, b2, dlt=0.1);res2
