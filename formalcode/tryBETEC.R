rm(list=ls())
library(clinfun)
library(optparse)
library(magrittr)
library(parallel)

#setwd("C:/Users/Dell/Documents/ProjectCode/BETEC/formalcode")
source("utilities.R")

option.list <- list(
    make_option("--p0", type="double", default=0.05, help="p0"),
    make_option("--p1", type="double", default=0.25, help="p1"),
    make_option("--a1", type="double", default=0.10, help="a1"),
    make_option("--a2", type="double", default=0.12, help="a2"),
    make_option("--N", type="integer", default=10000, help="Num of sample used in MCMC")
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
pi2 <- 0.55

a1 <- opt$a1
a2 <- opt$a2

res1 <- BETEC(p0, p1, pi1, pi2, a1, a2, alpha0=1, beta0=1, nMax=200, N=opt$N);res1
round(Combo.Results(res1, p0, p1), 4)



