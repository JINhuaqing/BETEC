#!/usr/bin/Rscript
rm(list=ls())
library(clinfun)
library(optparse)
library(magrittr)
library(parallel)

#setwd("C:/Users/Dell/Documents/ProjectCode/BETEC/formalcode")
source("utilities.R")

option.list <- list(
    make_option("--p0", type="double", default=0.5, help="p0"),
    make_option("--p1", type="double", default=0.7, help="p1"),
    make_option("--typ1", type="double", default=0.1, help="type I error"),
    make_option("--typ2", type="double", default=0.095, help="type II error")
                    )
opt <- parse_args(OptionParser(option_list=option.list))
           

print(opt)
# (p0, p1) = (0.05, 0.25), (alpha, beta) = (0.03, 0.04)
# (p0, p1) = (0.10, 0.30), (alpha, beta) = (0.06, 0.05)
# (p0, p1) = (0.20, 0.40), (alpha, beta) = (0.085, 0.08)
# (p0, p1) = (0.30, 0.50), (alpha, beta) = (0.095, 0.09)
# (p0, p1) = (0.50, 0.70), (alpha, beta) = (0.100, 0.095)

typ1 <- opt$typ1
typ2 <- opt$typ2
p0 <- opt$p0
p1 <- opt$p1

res <- ph2simon(p0, p1, typ1, typ2, nmax=100)
res
res <- c(10, 18, 27, 45)
stats <- round(Combo.Results(res, p0, p1, N=100000), 6)[c(11, 9, 6,  7, 4, 5, 1, 2, 3)]
c(res, stats)

fdsaf
res1 <- c(3, 28, 29, 70)
stats1 <- round(Combo.Results(res1, p0, p1, N=100000), 10)[c(11, 9, 6,  7, 4, 5, 1, 2, 3)]
c(res1, stats1)
