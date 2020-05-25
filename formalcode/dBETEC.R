rm(list=ls())
library(clinfun)
library(optparse)
library(magrittr)
library(parallel)

source("utilities.R")

option.list <- list(
    make_option("--p0", type="double", default=0.2, help="p0"),
    make_option("--p1", type="double", default=0.5, help="p1"),
    make_option("--b1", type="double", default=0.005, help="b1"),
    make_option("--b2", type="double", default=0.150, help="b2")
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
pi2 <- 0.60

b1 <- opt$b1
b2 <- opt$b2

res <- deltaBETEC(p0, p1, pi1, pi2, b1, b2, dlt=0.1, alpha0=alpha0, beta0=beta0);res
stats <- round(Combo.Results(res, p0, p1, N=100000), 6)
c(res, stats)




