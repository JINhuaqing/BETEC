library(magrittr)


# compute the probability of early termination w.r.t p, i.e. reject probability w.r.t p at stage 1
PET <- function(r1, n1, p){
  if (r1==0){
      return(0)
  }else{
  return(pbinom(r1-1, n1, p))
  }
}

# expected sample size
ESS <- function(r1, n1, n, p){
    pet <- PET(r1, n1, p)
    n1 + (1-pet) * (n - n1)
}

# compute the reject probability w.r.t p, for the whole design
rej.prob <- function(r1, r, n1, n, p){
  xs <- (r1):(min(n1, r-1))
  itm1 <- pbinom(r1-1, n1, p)
  itm2 <- sum(dbinom(xs, n1, p)*pbinom(r-1-xs, n-n1, p))
  return(itm1+itm2)
}

# compute the probability of response rate > p when observing (r, n)
post.prob <- function(alpha0, beta0, r, n, p){
    prob <- 1 - pbeta(p, alpha0+r, beta0+n-r)
    return(prob)
}


# compute the Pr(lb<p<ub|reject the drug)
int.post.density <- function(lb, ub, r1, r, n1, n, alpha0, beta0, N=10000){
    ps <- rbeta(N, alpha0, beta0)
    probs <- sapply(ps, rej.prob, r1=r1, r=r, n1=n1, n=n)
    idxkeep <- (ps <= ub) & (ps >= lb) 
    probs.truc <- probs * idxkeep
    mean(probs.truc)/mean(probs)
}

# compute the Pr(lb<p<ub|reject the drug at stage 1)
int.post.density.stage1 <- function(lb, ub, r1, n1, alpha0, beta0, N=10000){
    ps <- rbeta(N, alpha0, beta0)
    probs <- sapply(ps, PET, r1=r1, n1=n1)
    idxkeep <- (ps <= ub) & (ps >= lb) 
    probs.truc <- probs * idxkeep
    mean(probs.truc)/mean(probs)
}

# compute the parameters of stage 1 of bBET for Pr(p<p1|r1-1, n1)
bBET.stage1 <- function(p0, p1, pi1, a1, alpha0, beta0, nMax=100){
    res.s1 <- c()
    for (n1 in 1:nMax){
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
            print(paste("Current n1 is", n1, "Need larger n1"))
        }
    }
    res.s1
}


# compute the parameters of stage 2 of deltabBET 
deltabBET.stage2 <- function(r1, n1, p0, p1, pi2, a2, alpha0, beta0, nMax=200){
    res.s2 <- c()
    dlt <- p1 - p0
    for (n in (n1+1):nMax){
        probs.2l <- post.prob(alpha0, beta0, r1:(n-n1+r1), n, p1)
        #probs.2l <- post.prob(alpha0, beta0, r1:n, n, p1)
        probs.2s <- post.prob(alpha0, beta0, (r1-1):(n-n1+r1-1), n, p1+dlt)
        #probs.2s <- post.prob(alpha0, beta0, (r1-1):(n-1), n, p1+dlt)
        idxs.2 <- (probs.2l>pi2) + (probs.2s<a2)
        rrange <- r1:(n-n1+r1)
        #rrange <- r1:n
        rrange <- rrange[idxs.2==2]
        if (length(rrange) >= 1){
           res.s2 <- c(rrange[1], n) 
           print("Find addmissible solution")
           break()
        }else{
            print("Need larger n")
        }
    }
    res.s2
}


# compute the parameters of stage 1 of bBET for Pr(|R1)
bBET.stage1.R1 <- function(p0, p1, pi1, a1, alpha0, beta0, nMax=100, N=10000){
    res.s1 <- c()
    bflag <- 0
    for (n1 in 1:nMax){
        for (r1 in 1:n1){
            prob.l <- post.prob(alpha0, beta0, r1, n1, p0)
            prob.s <- int.post.density.stage1(p1, 1, r1, n1, alpha0, beta0, N=N)
            print(c(prob.l, prob.s))
            if (prob.l>pi1 & prob.s<a1){
                res.s1 <- c(r1, n1) 
                print("Find addmissible solution")
                bflag <- 1
                break()
    #         }else if (prob.s >= a1){
    #             break()
            }else{
                print(paste("Current (r1, n1) is", r1, n1, ".", "Need to continue"))
            }
        }
      if (bflag){
        break()
      }
    }
    res.s1
}

# compute the parameters of stage 2 of bBET 
bBET.stage2 <- function(r1, n1, p0, p1, pi2, a2, alpha0, beta0, nMax=200, N=10000){
    res.s2 <- c()
    bflag <- 0
    for (n in (n1+1):nMax){
        for (r in r1:(n-n1+r1)){
            prob.2l <- post.prob(alpha0, beta0, r, n, p1)
            prob.2s <- int.post.density(p1, 1, r1, r, n1, n, alpha0, beta0, N=N)
            print(c(prob.2l, prob.2s))
            if (prob.2l>pi2 & prob.2s<a2){
                res.s2 <- c(r, n) 
                print("Find addmissible solution")
                bflag <- 1
                break()
    #         }else if (prob.2s >= a2){
    #             break()
            }else{
                print(paste("Current (r, n) is", r, n, ".", "Need to continue"))
            }
        }
      if (bflag){
        break()
      }
    }
    res.s2
}


