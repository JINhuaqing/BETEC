rm(list=ls())
setwd("C:/Users/Dell/Google Drive/multi-computers_folder/projects/BET_ext")
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
a1 <- 0.09
a2 <- 0.22
N <- 50000


res.s1 <- bBET.stage1.R1(p0, p1, pi1, a1, alpha0, beta0, nMax=100, N=N)
n1 <- res.s1[2]
r1 <- res.s1[1]
res.s2 <- bBET.stage2(r1, n1, p0, p1, pi2, a2, alpha0, beta0, nMax=200, N=N)
res.s2

res <- data.frame(stage1=res.s1, stage2=res.s2)
rownames(res) <- c("r", "n")
res

#params <- list(
#    pi1=pi1, pi2=pi2, a1=a1, a2=a2, p0=p0, p1=p1, N=N
#)
#
#output.res <- list(result=res, params=params)
#save.name <- paste0("bBETR1", pi1*100, "_", pi2*100, "_", a1*100, "_",  a2*100, "_", p0*100, "_", p1*100, ".RData")
#save(output.res, file=save.name)


r <- res.s2[1]
n <- res.s2[2]
N <- 50000
PoPR1s <- int.post.density.stage1(p1, 1, r1, n1, alpha0, beta0, N)
PoPr1s <- 1-post.prob(alpha0, beta0, r1, n1, p0)
PoPRs <- int.post.density(p1, 1, r1, r, n1, n, alpha0, beta0, N)
PoPrs <- post.prob(alpha0, beta0, r, n, p1)
c(PoPR1s, PoPr1s, PoPRs, PoPrs) %>% round(3)
