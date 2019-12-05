rm(list=ls())
setwd("C:/Users/Dell/Google Drive/multi-computers_folder/projects/bBET")
source("utilities.R")
library(magrittr)
library(dplyr)

# Load the data for delta method
fs <- list.files(pattern="deltabBET.*.RData")
ress <- list()
for (i in 1:length(fs)){
    load(fs[i])
    ress[[i]] <- output.res
}

list2df <- function(res){
    res.res <- do.call(rbind, res$result) %>% as.data.frame()
    res.para <- do.call(rbind, res$corparas) %>% as.data.frame()
    cbind(res.res, res.para)
}

# compute the PoP of H_0, H_1 
PoP.combo <- function(res, alpha0, beta0, N=200000){
    PoPR1s <- c()
    PoPr1s <- c()
    PoPRs <- c()
    PoPrs <- c()
    p1 <- res$params$p1
    p0 <- res$params$p0
    for (re in res$result){
        r1 <- re[1]
        r <- re[3]
        n1 <- re[2]
        n <- re[4]
        PoPR1s <- c(PoPR1s, int.post.density.stage1(p1, 1, r1, n1, alpha0, beta0, N))
        PoPr1s <- c(PoPr1s, 1-post.prob(alpha0, beta0, r1, n1, p0))
        PoPRs <- c(PoPRs, int.post.density(p1, 1, r1, r, n1, n, alpha0, beta0, N))
        PoPrs <- c(PoPrs, post.prob(alpha0, beta0, r, n, p1))
    }
    res.df <- cbind(PoPR1s, PoPr1s, PoPRs, PoPrs) %>% as.data.frame()
    names(res.df) <- c("H1_R1", "H0_r1n1", "H1_R", "H1_rn")
    res.df
    
}


comb.res <- function(res, alpha0, beta0, N=10000){
    PoPvs <- PoP.combo(res, alpha0, beta0, N) %>% round(3)
    Paras <- list2df(res)
    res.df <- cbind(Paras, PoPvs)
    names(res.df) <- c("r1", "n1", "r", "n", "a1", "a2", names(PoPvs))
    res.df[, c(5, 6, 1:4, 7:10)]
}

list2df(ress[[4]])[c(1, 5, 6), ]
list2df(ress[[1]])[c(1, 5, 6), ]
list2df(ress[[2]])[c(1, 5, 6), ]
list2df(ress[[3]])[c(1, 5, 6), ]

list2df(ress[[4]])
list2df(ress[[1]])
list2df(ress[[2]])
list2df(ress[[3]])

N <- 50000
alpha0 <- 1
beta0 <- 1
comb.res(ress[[4]], alpha0, beta0, N)
comb.res(ress[[1]], alpha0, beta0, N)
comb.res(ress[[2]], alpha0, beta0, N)
comb.res(ress[[3]], alpha0, beta0, N)
comb.res(ress[[4]], alpha0, beta0, N)[c(1, 5, 6), ]
comb.res(ress[[1]], alpha0, beta0, N)[c(1, 5, 6), ]
comb.res(ress[[2]], alpha0, beta0, N)[c(1, 5, 6), ]
comb.res(ress[[3]], alpha0, beta0, N)[c(1, 5, 6), ]


