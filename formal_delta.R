library(magrittr)
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


pprob <- function(alpha0, beta0, r, n, p1){
    prob <- 1 - pbeta(p1, alpha0+r, beta0+n-r)
    return(prob)
}

for (n1 in 1:100){
    probs.l <- pprob(alpha0, beta0, 1:n1, n1, p0)
    probs.s <- pprob(alpha0, beta0, 0:(n1-1), n1, p1)
    #plot(1:n1, probs.l, type = "b", col="red", ylim = c(0, 1), pch=17)
    #lines(1:n1, probs.s, type = "b", col="green")
    #abline(h=c(0.10, 0.8))
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
#    plot(0:n, probs.2l, type="b", col="red", ylim=c(0, 1))
#    lines(0:n, probs.2s, type="b", col="green", ylim=c(0, 1), pch=17)
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

res <- data.frame(stage1=res.s1, stage2=res.s2)
rownames(res) <- c("r", "n")
res


