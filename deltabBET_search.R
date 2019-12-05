library(magrittr)
source("utilities.R")

myfun <- function(p0, p1, a1, a2, pi1, pi2, alpha0, beta0, nMin=1){
    res.s1 <- bBET.stage1(p0, p1, pi1, a1, alpha0, beta0, nMin=nMin, nMax=100)
    n1 <- res.s1[2]
    r1 <- res.s1[1]
    res.s2 <- deltabBET.stage2(r1, n1, p0, p1, pi2, a2, alpha0, beta0, nMax=200)

    res <- c(res.s1, res.s2)
    names(res) <- c("r1", "n1", "r", "n")
    res
}

alpha0 <- 4
beta0 <- 6
nMin <- 1

# Test probabilities
p0 <- 0.3
p1 <- 0.5

# 4 hyper-parameters
pi1 <- 0.8
pi2 <- 0.9
a1 <- 0.05
a2 <- 0.05

a1s <- c(0.05, 0.1, 0.15)
a2s <- c(0.05, 0.1, 0.15, 0.2)

flag <- 1
all.res <- list()
corparas <- list()
for (nowa1 in a1s){
    for (nowa2 in a2s){
        print(c(flag, nowa1, nowa2))
        nowres <- myfun(a1=nowa1, a2=nowa2, p0=p0, p1=p1, pi1=pi1, pi2=pi2, alpha0=alpha0, beta0=beta0, nMin=nMin)
        all.res[[flag]] <- nowres
        corparas[[flag]] <- c(nowa1, nowa2)
        flag <- flag+1
    }
}
params <- list(
    pi1=pi1, pi2=pi2, p0=p0, p1=p1
)

output.res <- list(result=all.res, corparas=corparas, params=params)
save.name <- paste0("infodeltabBET_all", p0*100, "_", p1*100, ".RData")
#save.name <- paste0("deltabBET_all", p0*100, "_", p1*100, ".RData")
save(output.res, file=save.name)



