rm(list=ls())
library(clinfun)
library(optparse)
library(magrittr)
library(parallel)

#setwd("C:/Users/Dell/Documents/ProjectCode/BETEC/formalcode")
source("utilities.R")

option.list <- list(
    make_option("--p0", type="double", default=0.1, help="p0, default is 0.05"),
    make_option("--p1", type="double", default=0.3, help="p1"),
    make_option("--a1", type="double", default=0.02, help="a1"),
    make_option("--a2", type="double", default=0.08, help="a2"),
    make_option("--N", type="integer", default=100000, help="Num of sample used in MCMC, default is 10000"),
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

stats <- round(Combo.Results(res, p0, p1, N=100000), 6)
