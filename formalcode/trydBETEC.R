#!/usr/bin/Rscript
rm(list=ls())
library(clinfun)
library(optparse)
library(magrittr)
library(parallel)

#setwd("C:/Users/Dell/Documents/ProjectCode/BETEC/formalcode")
source("utilities.R")

# (p0, p1) = (0.05, 0.25), (b1, b2)=(0.05, 0.055)
# (p0, p1) = (0.1, 0.3), (b1, b2)=(0.05, 0.055)
# (p0, p1) = (0.2, 0.4), (b1, b2)=(0.015, 0.07)
# (p0, p1) = (0.6, 0.8), (b1, b2)=(0.015, 0.015)
option.list <- list(
    make_option("--p0", type="double", default=0.6, help="p0"),
    make_option("--p1", type="double", default=0.8, help="p1"),
    make_option("--b1", type="double", default=0.015, help="b1"),
    make_option("--b2", type="double", default=0.015, help="b2")
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

b1 <- opt$b1
b2 <- opt$b2
# (p0, p1) = (0.05,0.25), (b1, b2)= (0.03, 0.58)
# (p0, p1) = (0.1,0.3), (b1, b2)= (0.03, 0.60)
# (p0, p1) = (0.2,0.4), (b1, b2)= (0.03, 0.70)
# (p0, p1) = (0.2,0.4), (b1, b2)= (0.03, 0.70)

res <- deltaBETEC(p0, p1, pi1, pi2, b1, b2, dlt=0.1);res
stats <- round(Combo.Results(res, p0, p1, N=100000), 6)[c(11, 9, 6,  7, 4, 5, 1, 2, 3)]
c(res, stats)



