library(magrittr)


# compute the probability of early termination w.r.t p, i.e. reject probability w.r.t p at stage 1
PET <- function(r1, n1, p){
  if (r1==0){
      return(0)
  }else{
  return(pbinom(r1-1, n1, p))
  }
}

# expected sample size w.r.t p 
ESS <- function(r1, n1, n, p){
    pet <- PET(r1, n1, p)
    n1 + (1-pet) * (n - n1)
}

# compute the probability of rejecting drug (accept H0) w.r.t p, for the whole design
rej.prob <- function(r1, r, n1, n, p){
  xs <- (r1):(min(n1, r-1))
  itm1 <- pbinom(r1-1, n1, p)
  itm2 <- sum(dbinom(xs, n1, p)*pbinom(r-1-xs, n-n1, p))
  return(itm1+itm2)
}

# compute the probability of response rate > p when observing (r, n)
post.prob <- function(alpha0, beta0, r, n, p){
    # Arguments:
    # (alpha0, beta0): the prior parameters of Beta distribution

    prob <- 1 - pbeta(p, alpha0+r, beta0+n-r)
    return(prob)
}


# compute the Pr(lb<p<ub|reject the drug)
int.post.density <- function(lb, ub, r1, r, n1, n, alpha0, beta0, N=10000){
    # Arguments:
    # (lb, ub): Lower bound and upper bound of the integration
    # (r1, r, n1, n): The design parameters for the two-stage design
    # (alpha0, beta0): The prior parameters of Beta distribution
    # N: The number of samples used in MCMC integration

    ps <- rbeta(N, alpha0, beta0)
    probs <- sapply(ps, rej.prob, r1=r1, r=r, n1=n1, n=n)
    idxkeep <- (ps <= ub) & (ps >= lb) 
    probs.truc <- probs * idxkeep
    mean(probs.truc)/mean(probs)
}

# compute the Pr(lb<p<ub|reject the drug at stage 1)
int.post.density.stage1 <- function(lb, ub, r1, n1, alpha0, beta0, N=10000){
    # Arguments:
    # (lb, ub): Lower bound and upper bound of the integration
    # (r1, n1): The design parameters for the two-stage design at stage 1
    # (alpha0, beta0): The prior parameters of Beta distribution
    # N: The number of samples used in MCMC integration
    ps <- rbeta(N, alpha0, beta0)
    probs <- sapply(ps, PET, r1=r1, n1=n1)
    idxkeep <- (ps <= ub) & (ps >= lb) 
    probs.truc <- probs * idxkeep
    mean(probs.truc)/mean(probs)
}

# Compute the Bayesian and frequentist statistics give (r1, n1, r, n)
Combo.Results <- function(paras, p0, p1, alpha0=1, beta0=1, N=10000){
    # paras: The design parameters of two-stage trials, (r1, n1, r, n)
    # p0: The uninteresting response rate
    # p1: The target response rate
    # (alpha0, beta0): The prior parameters of Beta distribution
    # N: The number of samples used in MCMC integration

    r1 <- paras[1]
    n1 <- paras[2]
    r <- paras[3]
    n <- paras[4]

    typeI.err <- 1-rej.prob(r1, r, n1, n, p0)
    typeII.err <- rej.prob(r1, r, n1, n, p1)
    ess0 <- ESS(r1, n1, n, p0)
    ess1 <- ESS(r1, n1, n, p1)
    pet0 <- PET(r1, n1, p0)
    pet1 <- PET(r1, n1, p1)
    PoP.H0r1 <- 1 - post.prob(alpha0, beta0, r1, n1, p0)
    PoP.H1r <- post.prob(alpha0, beta0, r, n, p1)
    PoP.H1R1 <- int.post.density.stage1(p1, 1, r1, n1, alpha0, beta0, N)
    PoP.H1R <- int.post.density(p1, 1, r1, r, n1, n, alpha0, beta0, N)

    res <- c(typeI.err, typeII.err, ess0, ess1, pet0, pet1, PoP.H0r1, PoP.H1r, 
             PoP.H1R1, PoP.H1R)
    names(res) <- c("type1err", "type2err", "ESS0", "ESS1", 
                    "PET0", "PET1", "PoPH0r1", "PoPH1r", "PoPH1R1", "PoPH1R")
    res
}

# Search the design parameters of stage 1 of deltaBETEC 
deltaBETEC.stage1 <- function(p0, p1, pi1, b1, alpha0, beta0, nMin=1, nMax=100){
    # Arguments:
    # p0: The uninteresting response rate
    # p1: The target response rate
    # pi1: The minimal posterior probability of p>p0 given (r1, n1), i.e., Pr(p>p0|r1, n1) 
    # b1: The maximal posterior error rate given (r1-1, n1), i.e.,  Pr(p>p1|r1-1, n1)
    # (alpha0, beta0): The prior parameters of Beta distribution
    # (nMin, nMax): The minimal and maximal sample size at stage 1

    res.s1 <- c()
    for (n1 in nMin:nMax){
        probs.l <- post.prob(alpha0, beta0, 1:n1, n1, p0)
        probs.s <- post.prob(alpha0, beta0, 0:(n1-1), n1, p1)
        idxs <- (probs.l>pi1) + (probs.s<b1) 
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


# Search the design parameters of stage 2 of deltaBETEC
deltaBETEC.stage2 <- function(r1, n1, p0, p1, pi2, b2, dlt, alpha0, beta0, nMax=200){
    # Arguments:
    # (r1, n1): The parameters in the first stage
    # p0: The uninteresting response rate
    # p1: The target response rate
    # pi2: The minimal posterior probability of p>p1 given (r,n), i.e., Pr(p>p1|r, n) 
    # b2: The maximal posterior error rate given (r-1, n), i.e.,  Pr(p>p1+delta|r-1, n)
    # dlt: The delta in the second stage
    # (alpha0, beta0): The prior parameters of Beta distribution
    # nMax: The minimal and maximal sample size at stage 2

    res.s2 <- c()
    if (missing(dlt)){
        dlt <- p1 - p0
    }
    for (n in (n1+1):nMax){
        probs.2l <- post.prob(alpha0, beta0, r1:n, n, p1)
        probs.2s <- post.prob(alpha0, beta0, (r1-1):(n-1), n, p1+dlt)
        idxs.2 <- (probs.2l>pi2) + (probs.2s<b2)
        rrange <- r1:n
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


# Search parameters of stage 1 of BETEC 
BETEC.stage1 <- function(p0, p1, pi1, a1, alpha0, beta0, nMin=1, nMax=100, N=10000){
    # Arguments:
    # p0: The uninteresting response rate
    # p1: The target response rate
    # pi1: The minimal posterior probability of p>p0 given (r1, n1), i.e., Pr(p>p0|r1, n1) 
    # a1: The maximal posterior error rate given rejecting drug at stage 1, i.e.,  Pr(p>p1|<r1, n1)
    # (alpha0, beta0): The prior parameters of Beta distribution
    # (nMin, nMax): The minimal and maximal sample size at stage 2
    # N: The number of samples used in MCMC integration

    res.s1 <- c()
    bflag <- 0
    for (n1 in nMin:nMax){
        for (r1 in 1:n1){
            prob.l <- post.prob(alpha0, beta0, r1, n1, p0)
            prob.s <- int.post.density.stage1(p1, 1, r1, n1, alpha0, beta0, N=N)
            print(c(prob.l, prob.s))
            if (prob.l>pi1 & prob.s<a1){
                res.s1 <- c(r1, n1) 
                print("Find addmissible solution")
                bflag <- 1
                break()
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

# Search the design parameters of stage 2 of BETEC
BETEC.stage2 <- function(r1, n1, p0, p1, pi2, a2, alpha0, beta0, nMax=200, N=10000){
    # Arguments:
    # p0: The uninteresting response rate
    # p1: The target response rate
    # pi2: The minimal posterior probability of p>p1 given (r, n), i.e., Pr(p>p1|r, n) 
    # a2: The maximal posterior error rate given rejecting drug, i.e.,  Pr(p>p1|R)
    # (alpha0, beta0): The prior parameters of Beta distribution
    # nMax: The maximal sample size at stage 2
    # N: The number of samples used in MCMC integration
    res.s2 <- c()
    bflag <- 0
    for (n in (n1+1):nMax){
        for (r in r1:n){
            prob.2l <- post.prob(alpha0, beta0, r, n, p1)
            prob.2s <- int.post.density(p1, 1, r1, r, n1, n, alpha0, beta0, N=N)
            print(c(prob.2l, prob.2s))
            if (prob.2l>pi2 & prob.2s<a2){
                res.s2 <- c(r, n) 
                print("Find addmissible solution")
                bflag <- 1
                break()
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


# Search the design parameters of BETEC method
BETEC <- function(p0, p1, pi1, pi2, a1, a2, alpha0=1, beta0=1, nMax=200, N=10000){
    # Arguments:
    # p0: The uninteresting response rate
    # p1: The target response rate
    # pi1: The minimal posterior probability of p>p0 given (r1, n1), i.e., Pr(p>p0|r1, n1) 
    # pi2: The minimal posterior probability of p>p1 given (r, n), i.e., Pr(p>p1|r, n) 
    # a1: The maximal posterior error rate given rejecting drug at stage 1, i.e.,  Pr(p>p1|<r1, n1)
    # a2: The maximal posterior error rate given rejecting drug, i.e.,  Pr(p>p1|R)
    # (alpha0, beta0): The prior parameters of Beta distribution. Default is (1, 1)
    # nMax: The maximal sample size at stage 2. Default is 200
    # N: The number of samples used in MCMC integration
    res.s1 <- BETEC.stage1(p0, p1, pi1, a1, alpha0, beta0, nMin=1, nMax=as.integer(nMax/3), N=N)
    n1 <- res.s1[2]
    r1 <- res.s1[1]
    res.s2 <- BETEC.stage2(r1, n1, p0, p1, pi2, a2, alpha0, beta0, nMax=nMax, N=N)
    res <- c(res.s1, res.s2)
    names(res) <- c("r1", "n1", "r", "n")
    res
}


# Search the design parameters of BETEC method
deltaBETEC <- function(p0, p1, pi1, pi2, b1, b2, dlt, alpha0=1, beta0=1, nMax=200){
    # Arguments:
    # p0: The uninteresting response rate
    # p1: The target response rate
    # pi1: The minimal posterior probability of p>p0 given (r1, n1), i.e., Pr(p>p0|r1, n1) 
    # pi2: The minimal posterior probability of p>p1 given (r, n), i.e., Pr(p>p1|r, n) 
    # b1: The maximal posterior error rate given (r1-1, n1), i.e.,  Pr(p>p1|r1-1, n1)
    # b2: The maximal posterior error rate given (r-1, n), i.e.,  Pr(p>p1+delta|r-1, n)
    # dlt: The delta in the second stage
    # (alpha0, beta0): The prior parameters of Beta distribution. Default is (1, 1)
    # nMax: The maximal sample size at stage 2. Default is 200
    res.s1 <- deltaBETEC.stage1(p0, p1, pi1, b1, alpha0, beta0, nMin=1, nMax=as.integer(nMax/3))
    n1 <- res.s1[2]
    r1 <- res.s1[1]
    if (missing(dlt)){
        dlt <- p1 - p0
    }
    res.s2 <- deltaBETEC.stage2(r1, n1, p0, p1, pi2, b2, dlt=dlt, alpha0=alpha0, beta0=beta0, nMax=nMax)
    res <- c(res.s1, res.s2)
    names(res) <- c("r1", "n1", "r", "n")
    res
}
