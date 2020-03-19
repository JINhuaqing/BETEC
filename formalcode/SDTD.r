# R code to implement the STD and DTD design from (Tan_Machin_SIM_2002)

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

pi.prior <- 0.1
Ru <- 0.45
Rl <- 0.25
lam1 <- 0.7
lam2 <- 0.8
eps <- 0.05
alp0 <- pi.prior + 1
bet0 <- (1-pi.prior) + 1

Rue <- Ru + eps

res.DTD <- DTD(Ru, Rl, lam1, lam2, alp0, bet0, eps)
n <- res.DTD[1]
N <- res.DTD[2]
demores.DTD <- DTD2BD(n, N, Ru, Rl, lam1, lam2, alp0, bet0)
res <- STD(Ru, lam1, lam2, alp0, bet0, eps)
n <- res[1]
N <- res[2]
demores <- STD2BD(n, N, Ru, lam1, lam2, alp0, bet0) 


