rm(list=ls())
library(clinfun)
library(optparse)
library(magrittr)
library(parallel)

#setwd("C:/Users/Dell/Documents/ProjectCode/BETEC/formalcode")
source("utilities.R")

option.list <- list(
    make_option("--p0", type="double", default=0.5, help="p0, default is 0.05"),
    make_option("--p1", type="double", default=0.7, help="p1"),
    make_option("--a1", type="double", default=0.01, help="a1"),
    make_option("--a2", type="double", default=0.05, help="a2"),
    make_option("--N", type="integer", default=10000, help="Num of sample used in MCMC, default is 10000"),
    make_option("--pi2", type="double", default=0.55, help="pi2")
                    )
opt <- parse_args(OptionParser(option_list=option.list))
           

print(opt)




alpha0 <- 1
beta0 <- 1
nMin <- 1

# Test probabilities
p0 <- opt$p0
p1 <- opt$p1

# 4 hyper-parameters
pi1 <- 0.50
pi2 <- opt$pi2

a1 <- opt$a1
a2 <- opt$a2

res1 <- BETEC(p0, p1, pi1, pi2, a1, a2, alpha0=1, beta0=1, nMax=200, N=opt$N);res1

#print(opt)
#round(Combo.Results(res1, p0, p1), 4)

##################################################
# (p0, p1) = (0.05, 0.25)
# (a1, a2) = (0.1, 0.12) (r1, n1, r, n) = (1, 8, 9, 36)
# (a1, a2) = (0.06, 0.11) (r1, n1, r, n) = (1, 9, 12, 47)

##################################################
# (p0, p1) = (0.10, 0.30)
# (a1, a2) = (0.1, 0.11) (r1, n1, r, n) = (1, 6, 14, 46) typII = 0.5
# (a1, a2) = (0.08, 0.1) (r1, n1, r, n) = (1, 7, 14, 46) typII = 0.49

##################################################
# (p0, p1) = (0.20, 0.40)
# (a1, a2) = (0.03, 0.08) (r1, n1, r, n) = (1, 6, 18, 44) typII = 0.502
# (a1, a2) = (0.01, 0.075) (r1, n1, r, n) = (2, 12, 18, 44) typII = 0.493

##################################################
# (p0, p1) = (0.30, 0.50)
# (a1, a2) = (0.01, 0.06) (r1, n1, r, n) = (4, 13, 23, 45) typII = 0.504

##################################################
# (p0, p1) = (0.40, 0.60)
# (a1, a2) = (0.01, 0.05) (r1, n1, r, n) = (4, 10, 27, 44) typII = 0.515

##################################################
# (p0, p1) = (0.50, 0.70)
# (a1, a2) = (0.008, 0.041) (r1, n1, r, n) = (7, 13, 33, 46) typII = 0.523

##################################################
# (p0, p1) = (0.60, 0.80)
# (a1, a2) = (0.004, 0.03) (r1, n1, r, n) = (7, 11, 36, 44) typII = 0.533

##################################################
# (p0, p1) = (0.70, 0.90)
# (a1, a2) = (0.001, 0.018) (r1, n1, r, n) = (8, 11, 44, 48) typII = 0.523

res <- c(8, 11, 44, 48)
stats <- round(Combo.Results(res, 0.7, 0.9, N=100000), 6)[c(11, 9, 6,  7, 4, 5, 1, 2, 3)]
c(res, stats)
#round(Combo.Results(c(8, 11, 44, 48), 0.7, 0.93), 4)
