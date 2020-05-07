#!/usr/bin/Rscript
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
    make_option("--typ1", type="double", default=0.1, help="type I error"),
    make_option("--typ2", type="double", default=0.60, help="type II error")
                    )
opt <- parse_args(OptionParser(option_list=option.list))
           

print(opt)


typ1 <- opt$typ1
typ2 <- opt$typ2
p0 <- opt$p0
p1 <- opt$p1


res <- ph2simon(p0, p1, typ1, typ2, nmax=100)
print(res)
#round(Combo.Results(res, p0, p1), 4)



