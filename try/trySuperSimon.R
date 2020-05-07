#!/usr/bin/Rscript
rm(list=ls())
library(clinfun)
library(optparse)
library(magrittr)
library(parallel)

#setwd("C:/Users/Dell/Documents/ProjectCode/BETEC/formalcode")
source("utilities.R")

option.list <- list(
    make_option("--p0", type="double", default=0.6, help="p0"),
    make_option("--p1", type="double", default=0.8, help="p1"),
    make_option("--typ1", type="double", default=0.05, help="type I error"),
    make_option("--typ2", type="double", default=0.05, help="type II error")
                    )
opt <- parse_args(OptionParser(option_list=option.list))
           

print(opt)
# (p0, p1) = (0.05, 0.25), (alpha, beta) = (0.09, 0.09)
# (p0, p1) = (0.10, 0.30), (alpha, beta) 
# (p0, p1) = (0.20, 0.40), (alpha, beta) = (0.100, 0.10)
# (p0, p1) = (0.30, 0.50), (alpha, beta) = (0.100, 0.095)
# (p0, p1) = (0.40, 0.60), (alpha, beta) = (0.090, 0.090)
# (p0, p1) = (0.50, 0.70), (alpha, beta) = (0.050, 0.050)


typ1 <- opt$typ1
typ2 <- opt$typ2
p0 <- opt$p0
p1 <- opt$p1


res <- ph2simon(p1, p1+p1-p0-0.05, typ1, typ2, nmax=100)
res

res <- c(14, 18, 36, 44)
stats <- round(Combo.Results(res, p0, p1, N=100000), 10)[c(11, 9, 6,  7, 4, 5, 1, 2, 3)]
c(res, stats)



