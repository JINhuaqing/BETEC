library(magrittr)

pprob <- function(alpha0, beta0, r, n, p1){
    prob <- 1 - pbeta(p1, alpha0+r, beta0+n-r)
    return(prob)
}


myfun <- function(p0, p1, a1, a2, pi1, pi2, dlt, alpha0, beta0){
    for (n1 in 1:100){
        probs.l <- pprob(alpha0, beta0, 1:n1, n1, p0)
        probs.s <- pprob(alpha0, beta0, 0:(n1-1), n1, p1)
        idxs <- (probs.l>pi1) + (probs.s<a1) 
        r1range <- 1:n1
        r1range <- r1range[idxs==2];
        print(n1)
        if (length(r1range) >= 1){
            res.s1 <- c(r1range[1], n1)
            break()
        }
    }
    
    n1 <- res.s1[2]
    r1 <- res.s1[1]
    
    for (n in (n1+1):200){
        probs.2l <- pprob(alpha0, beta0, r1:(n-n1+r1), n, p1)
        probs.2s <- pprob(alpha0, beta0, (r1-1):(n-n1+r1-1), n, p1+dlt)
        idxs.2 <- (probs.2l>pi2) + (probs.2s<a2)
        rrange <- r1:(n-n1+r1)
        rrange <- rrange[idxs.2==2]
        if (length(rrange) >= 1){
           res.s2 <- c(rrange[1], n) 
           print("Find addmissible solution")
           break()
        }else{
            print("Need larger n")
        }
    }
    res <- c(res.s1, res.s2)
    names(res) <- c("r1", "n1", "r", "n")
    res
}

alpha0 <- 1
beta0 <- 1

# Test probabilities
p0 <- 0.3
p1 <- 0.5
dlt <- 0.2

# 4 hyper-parameters
pi1 <- 0.8
pi2 <- 0.9
a1 <- 0.05
a2 <- 0.05

a1s <- seq(0.01, 0.2, 0.01)
a2s <- seq(0.01, 0.20, 0.01)

flag <- 1
all.res <- list()
corparas <- list()
for (nowa1 in a1s){
    for (nowa2 in a2s){
        print(c(flag, nowa1, nowa2))
        nowres <- myfun(a1=nowa1, a2=nowa2, p0=p0, p1=p1, pi1=pi1, pi2=pi2, alpha0=alpha0, beta0=beta0, dlt=dlt)
        all.res[[flag]] <- nowres
        corparas[[flag]] <- c(nowa1, nowa2)
        flag <- flag+1
    }
}
params <- list(
    pi1=pi1, pi2=pi2, p0=p0, p1=p1
)

output.res <- list(result=all.res, corparas=corparas, params=params)
save.name <- paste0("ResultDelta", p0*100, "_", p1*100, ".RData")
save(output.res, file=save.name)



