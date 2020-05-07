# R code to implement the STD and DTD design from (Tan_Machin_SIM_2002)
source("utilities.R")

# Posterior probability of right tail (Pr(>Ru|D))
PoP.RT <- function(Ru, num, Rue, alp0, bet0){
    # Arguments:
    # Ru: The target response rate
    # num: n or N
    # Rue: Ru + eps
    # (alp0, bet0): Parameters for beta prior
    y <- num * Rue
    1 - pbeta(Ru, shape1=y+alp0, shape2=num-y+bet0)
}

STD <- function(Ru, lam1, lam2, alp0, bet0, eps){
    # Arguments:
    # Ru: The target response rate
    # lam1: The cutoff value of stage 1
    # lam2: The cutoff value of stage 2
    # (alp0, bet0): Parameters for beta prior
    # eps: Ru+eps
    
    flag <- 0
    Rue <- Ru + eps
    if ((PoP.RT(Ru, 9, Rue, alp0, bet0) > lam2) & (PoP.RT(Ru, 4, Rue, alp0, bet0) > lam1))
    {
        res <- c(4, 9)
        names(res) <- c("n", "N")
        return(res)
    }
    for (N in 10:90){
        if (PoP.RT(Ru, N, Rue, alp0, bet0) <= lam2){
            next
        }else{
            for (n in 5:(N-5)){
                #print(c(n, N, PoP.RT(Ru, N, Rue, alp0, bet0), PoP.RT(Ru, n, Rue, alp0, bet0)))
                if (PoP.RT(Ru, n, Rue, alp0, bet0) > lam1){
                    res <- c(n, N)
                    names(res) <- c("n", "N")
                    flag <- 1
                    break
                }else{
                    next
                }
            }           
        }
        if (flag==1)
            break
    }
    
    if (flag==1){
        return(res)
    }else{
        return(c())
    }
    
}

STD2BD <- function(n, N, Ru, lam1, lam2, alp0, bet0){
    # Arguments:
    # n: Number of sample at stage 1
    # N: Number of sample at stage 1 and stage 2
    # Ru: The target response rate
    # lam1: The cutoff value of stage 1
    # lam2: The cutoff value of stage 2
    # (alp0, bet0): Parameters for beta prior
    for (y1 in 1:n){
        PoP.rt <- 1 - pbeta(Ru, shape1=y1+alp0, shape2=n-y1+bet0)
        if (PoP.rt >= lam1){
            r1 <- y1
            break()
        }
    }
    for (y in 1:N){
        PoP.rt <- 1 - pbeta(Ru, shape1=y+alp0, shape2=N-y+bet0)
        if (PoP.rt >= lam2){
            r <- y
            break()
        }
    }
    res <- c(r1, n, r, N)
    names(res) <- c("r1", "n", "r", "N")
    res
}



DTD <- function(Ru, Rl, lam1, lam2, alp0, bet0, eps){
    # Arguments:
    # Ru: The target response rate
    # Rl: The uninteresting response rate
    # lam1: The cutoff value of stage 1
    # lam2: The cutoff value of stage 2
    # (alp0, bet0): Parameters for beta prior
    # eps: Ru+eps
    
    flag <- 0
    Rue <- Ru + eps
    if ((PoP.RT(Ru, 9, Rue, alp0, bet0) > lam2) & (PoP.RT(Ru, 4, Rue, alp0, bet0) > lam1))
    {
        res <- c(4, 9)
        names(res) <- c("n", "N")
        return(res)
    }
    for (N in 10:90){
        if (PoP.RT(Ru, N, Rue, alp0, bet0) <= lam2){
            next
        }else{
            for (n in 5:(N-5)){
                #print(c(n, N, PoP.RT(Ru, N, Rue, alp0, bet0), PoP.RT(Ru, n, Rue, alp0, bet0)))
                if (PoP.RT(Rl, n, Rl-eps, alp0, bet0) < (1-lam1)){
                    res <- c(n, N)
                    names(res) <- c("n", "N")
                    flag <- 1
                    break
                }else{
                    next
                }
            }           
        }
        if (flag==1)
            break
    }
    
    if (flag==1){
        return(res)
    }else{
        return(c())
    }
    
}


DTD2BD <- function(n, N, Ru, Rl, lam1, lam2, alp0, bet0){
    # Arguments:
    # n: Number of sample at stage 1
    # N: Number of sample at stage 1 and stage 2
    # Ru: The target response rate
    # Rl: The uninteresting response rate
    # lam1: The cutoff value of stage 1
    # lam2: The cutoff value of stage 2
    # (alp0, bet0): Parameters for beta prior
    for (y1 in 1:n){
        PoP.lt <- pbeta(Rl, shape1=y1+alp0, shape2=n-y1+bet0)
        if (PoP.lt <= lam1){
            r1 <- y1
            break()
        }
    }
    for (y in 1:N){
        PoP.rt <- 1 - pbeta(Ru, shape1=y+alp0, shape2=N-y+bet0)
        if (PoP.rt >= lam2){
            r <- y
            break()
        }
    }
    res <- c(r1, n, r, N)
    names(res) <- c("r1", "n", "r", "N")
    res
}

# p0   p1   lam2
# 0.05 0.25 0.855
# 0.10 0.30 0.835
# 0.20 0.40 0.805
# 0.30 0.50 0.78
# 0.40 0.60 0.765
# 0.50 0.70 0.76
# 0.60 0.80 0.77
pi.prior <- 0.9
p0 <- 0.05
p1 <- 0.25
lam1 <- 0.6
lam2 <- 0.855
eps <- 0.05
alp0 <- pi.prior + 1
bet0 <- (1-pi.prior) + 1

Rue <- p1 + eps

res.DTD <- DTD(p1, p0, lam1, lam2, alp0, bet0, eps)
n <- res.DTD[1]
N <- res.DTD[2]
res <- DTD2BD(n, N, p1, p0, lam1, lam2, alp0, bet0)

stats <- round(Combo.Results(res, p0, p1, N=100000), 10)[c(11, 9, 6,  7, 4, 5, 1, 2, 3)]
c(res, stats)



