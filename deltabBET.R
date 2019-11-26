library(magrittr)
source("utilities.R")
alpha0 <- 1
beta0 <- 1

# Test probabilities
p0 <- 0.05
p1 <- 0.25

# 4 hyper-parameters
pi1 <- 0.8
pi2 <- 0.9
a1 <- 0.05
a2 <- 0.05

res.s1 <- bBET.stage1(p0, p1, pi1, a1, alpha0, beta0, nMax=100)
n1 <- res.s1[2]
r1 <- res.s1[1]
res.s2 <- deltabBET.stage2(r1, n1, p0, p1, pi2, a2, alpha0, beta0, nMax=200)



res <- data.frame(stage1=res.s1, stage2=res.s2)
rownames(res) <- c("r", "n")
res


