rm(list=ls())

library(magrittr)
library(parallel)
source("utilities.R")



myfun <- function(p0, p1, pi1, pi2, a1, a2, alpha0, beta0, nMin=1){
    N <- 200000
    res.s1 <- bBET.stage1.R1(p0, p1, pi1, a1, alpha0, beta0, nMin=nMin, nMax=100, N=N)
    n1 <- res.s1[2]
    r1 <- res.s1[1]
    res.s2 <- bBET.stage2(r1, n1, p0, p1, pi2, a2, alpha0, beta0, nMax=200, N=N)
    res.s2
    res <- c(res.s1, res.s2)
    names(res) <- c("r1", "n1", "r", "n")
    res
}

alpha0 <- 4
beta0 <- 6
nMin <- 8

# Test probabilities
p0 <- 0.2
p1 <- 0.4

# 4 hyper-parameters
pi1 <- 0.8
pi2 <- 0.90

a1s <- c(0.05, 0.1)
a2s <- c(0.20, 0.25)
#a2s <- c(0.20, 0.25, 0.30)

runfn <- function(i){
    nowa2 <- a2s[i]
    flag <- 1
    all.res <- list()
    corparas <- list()
    for (nowa1 in a1s){
        print(c(i, flag, nowa1, nowa2))
        nowres <- myfun(a1=nowa1, a2=nowa2, p0=p0, p1=p1, pi1=pi1, pi2=pi2, alpha0=alpha0, beta0=beta0, nMin=nMin)
        all.res[[flag]] <- nowres
        corparas[[flag]] <- c(nowa1, nowa2)
        flag <- flag+1
    }
    return(list(all.res, corparas))
}

params <- list(
    pi1=pi1, pi2=pi2, p0=p0, p1=p1
)

results <- mclapply(1:length(a2s), runfn, mc.cores=3)
output.res <- list(results=results, params=params)

save.name <- paste0("infobBET_all", p0*100, "_", p1*100, ".RData")
#save.name <- paste0("bBET_all", p0*100, "_", p1*100, ".RData")
save(output.res, file=save.name)


