rm(list=ls())
#setwd("C:/Users/Dell/Google Drive/multi-computers_folder/projects/BET_ext")
library(magrittr)
source("utilities.R")
alpha0 <- 1
beta0 <- 1

# Test probabilities
p0 <- 0.2
p1 <- 0.5

# 4 hyper-parameters
pi1 <- 0.91
pi2 <- 0.91
a1 <- 0.15
a2 <- 0.01

res.s1 <- bBET.stage1(p0, p1, pi1, a1, alpha0, beta0, nMax=100)
n1 <- res.s1[2]
r1 <- res.s1[1]
res.s2 <- deltabBET.stage2(r1, n1, p0, p1, pi2, a2, alpha0, beta0, nMax=200)



res <- data.frame(stage1=res.s1, stage2=res.s2)
rownames(res) <- c("r", "n")
res


r <- res.s2[1]
n <- res.s2[2]
N <- 50000
PoPR1s <- int.post.density.stage1(p1, 1, r1, n1, alpha0, beta0, N)
PoPr1s <- 1-post.prob(alpha0, beta0, r1, n1, p0)
PoPRs <- int.post.density(p1, 1, r1, r, n1, n, alpha0, beta0, N)
PoPrs <- post.prob(alpha0, beta0, r, n, p1)
c(PoPR1s, PoPr1s, PoPRs, PoPrs) %>% round(3)
