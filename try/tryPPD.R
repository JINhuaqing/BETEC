library(ph2bayes)
source("utilities.R")

theta <- 0.05
theta_t <- 0.90
alpha0 <- 1
beta0 <- 1
p0 <- 0.60
p1 <- 0.8
type <- "futility"
nmax <- 45
res <- stopbound_pred(theta, type, nmax, alpha0,
                      beta0, p0, theta_t)
n1 <- 15
res[[2]] <- res[[2]] + 1
idx1 <- res[[1]] - n1
nx <- res[[1]][idx1 >= 0][1]
if (nx == n1){
    r1 <- res[[2]][idx1 >= 0][1]
}else{
    r1 <- res[[2]][idx1 >= 0][1]
    r1 <- r1 -1 
}


idx <- res[1] - 45
n <- res[[1]][idx >= 0][1]
r <- res[[2]][idx >= 0][1]
res <- c(r1, n1, r, n)
res


#res <- c(2, 10, 13, 46)
stats <- round(Combo.Results(res, p0, p1, N=100000), 6)[c(11, 9, 6,  7, 4, 5, 1, 2, 3)]
c(res, stats)
