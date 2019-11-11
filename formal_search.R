rm(list=ls())
library(magrittr)


# compute the probability of early termination w.r.t p
PET <- function(r1, n1, p){
  return(pbinom(r1, n1, p))
}

# compute the reject probability w.r.t p
rej.prob <- function(r1, r, n1, n, p){
  xs <- (r1+1):(min(n1, r))
  itm1 <- pbinom(r1, n1, p)
  itm2 <- sum(dbinom(xs, n1, p)*pbinom(r-xs, n-n1, p))
  return(itm1+itm2)
}

# compute the probability of p0 > p when observing (r, n)
post.prob <- function(alpha0, beta0, r, n, p){
    prob <- 1 - pbeta(p, alpha0+r, beta0+n-r)
    return(prob)
}

# compute the probability to reject the drug 
prior.rej.prob <- function(r1, r, n1, n, 
                           alpha0=1, beta0=1, N=10000){
    ps <- rbeta(N, alpha0, beta0)
    probs <- sapply(ps, rej.prob, r1=r1, r=r, n1=n1, n=n)
    return(mean(probs))
}

# compute the posterior density of p when rejecting the drug
post.density <- function(p, r1, r, n1, n, mP,
                         prior=function(p){dbeta(p, alpha0, beta0)}){
    lkh <- rej.prob(r1, r, n1, n, p) 
    pr <- prior(p)
    return(lkh*pr/mP)
}
  
# compute the Pr(lb<p<ub|reject the drug)
int.post.density <- function(lb, ub, r1, r, n1, n, mP, N=10000){
    ps <- rbeta(N, alpha0, beta0)
    post.wp <- function(p){
      if (p>lb & p < ub){
        return(post.density(p, r1, r, n1, n, mP))
      }else{
        return(0)
      }
    }
    tr.probs <- sapply(ps, post.wp)
    return(mean(tr.probs))
}
    


myfun <- function(p0, p1, pi1, pi2, a1, a2, alpha0, beta0){
    N <- 20000
    for (n1 in 1:100){
        probs.l <- post.prob(alpha0, beta0, 1:n1, n1, p0)
        probs.s <- post.prob(alpha0, beta0, 0:(n1-1), n1, p1)
        idxs <- (probs.l>pi1) + (probs.s<a1) 
        r1range <- 1:n1
        r1range <- r1range[idxs==2];
        if (length(r1range) >= 1){
            res.s1 <- c(r1range[1], n1)
            print("Find addmissible solution")
            break()
        }else{
            x <- 1
            #print(paste("Current n1 is", n1, "Need larger n1"))
        }
    }
    
    n1 <- res.s1[2]
    r1 <- res.s1[1]
    
    bflag <- 0
    for (n in (n1+1):200){
        for (r in r1:(n-n1+r1)){
            prob.2l <- post.prob(alpha0, beta0, r, n, p1)
            mP <- prior.rej.prob(r1, r, n1, n, N=N, alpha0=alpha0, beta0=beta0)
            prob.2s <- int.post.density(p1, 1, r1, r, n1, n, mP, N=N)
            #print(c(prob.2l, prob.2s))
            if (prob.2l>pi2 & prob.2s<a2){
                res.s2 <- c(r, n) 
                print("Find addmissible solution")
                bflag <- 1
                break()
    #         }else if (prob.2s >= a2){
    #             break()
            }else{
                x <- 1
                #print(paste("Current (r, n) is", r, n, ".", "Need to continue"))
            }
        }
      if (bflag){
        break()
      }
    }
    
    res <- c(res.s1, res.s2)
    names(res) <- c("r1", "n1", "r", "n")
    res
}

alpha0 <- 1
beta0 <- 1

# Test probabilities
p0 <- 0.05
p1 <- 0.25

# 4 hyper-parameters
pi1 <- 0.8
pi2 <- 0.90
a1 <- 0.07
a2 <- 0.25

a1s <- c(0.03, 0.04, 0.06, 0.08, 0.09, 0.10)
a2s <- seq(0.20, 0.30, 0.01)
flag <- 1
all.res <- list()
corparas <- list()
for (nowa1 in a1s){
    for (nowa2 in a2s){
        print(c(flag, nowa1, nowa2))
        nowres <- myfun(a1=nowa1, a2=nowa2, p0=p0, p1=p1, pi1=pi1, pi2=pi2, alpha0=alpha0, beta0=beta0)
        all.res[[flag]] <- nowres
        corparas[[flag]] <- c(nowa1, nowa2)
        flag <- flag+1
    }
}
params <- list(
    pi1=pi1, pi2=pi2, p0=p0, p1=p1, N=N
)

output.res <- list(result=all.res, corparas=corparas, params=params)
save.name <- paste0("ResultBoth", p0*100, "_", p1*100, ".RData")
save(output.res, file=save.name)


