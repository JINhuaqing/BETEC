rm(list=ls())
library(magrittr)
source("utilities.R")



alpha0 <- 1
beta0 <- 1

# Test probabilities
p0 <- 0.1
p1 <- 0.3

# 4 hyper-parameters
pi1 <- 0.8
pi2 <- 0.90
a1 <- 0.07
a2 <- 0.25
N <- 20000


res.s1 <- bBET.stage1.R1(p0, p1, pi1, a1, alpha0, beta0, nMax=100, N=N)
n1 <- res.s1[2]
r1 <- res.s1[1]
res.s2 <- bBET.stage2(r1, n1, p0, p1, pi2, a2, alpha0, beta0, nMax=200, N=N)
res.s2

res <- data.frame(stage1=res.s1, stage2=res.s2)
rownames(res) <- c("r", "n")
res

params <- list(
    pi1=pi1, pi2=pi2, a1=a1, a2=a2, p0=p0, p1=p1, N=N
)

output.res <- list(result=res, params=params)
save.name <- paste0("bBETR1", pi1*100, "_", pi2*100, "_", a1*100, "_",  a2*100, "_", p0*100, "_", p1*100, ".RData")
save(output.res, file=save.name)


