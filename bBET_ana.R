rm(list=ls())
#setwd("C:/Users/Dell/Google Drive/multi-computers_folder/projects/BET_ext")
source("utilities.R")
library(magrittr)
library(dplyr)

# Load the data for delta method
fs <- list.files(pattern="infobBET.*.RData")
ress <- list()
for (i in 1:length(fs)){
    load(fs[i])
    ress[[i]] <- output.res
}

list2df <- function(res){
    res <- res$results
    df1 <- lapply(res, function(x){do.call(rbind, x[[1]])}) %>% do.call(rbind, args=.)
    df2 <- lapply(res, function(x){do.call(rbind, x[[2]])}) %>% do.call(rbind, args=.)
    df.all <- cbind(df1, df2) %>% as.data.frame()
    names(df.all) <- c("r1", "n1", "r", "n", "a1", "a2")
    df.all
}

# compute the PoP of H_0, H_1 
comb.res <- function(res, alpha0, beta0, N=10000){
    PoPR1s <- c()
    PoPr1s <- c()
    PoPRs <- c()
    PoPrs <- c()
    tb <- list2df(res)
    tb1 <- tb[, 1:4] %>% as.matrix()
    p1 <- res$params$p1
    p0 <- res$params$p0
    num <- dim(tb1)[1]
    for (i in 1:num){
        re <- tb1[i, ] 
        r1 <- re[1]
        r <- re[3]
        n1 <- re[2]
        n <- re[4]
        PoPR1s <- c(PoPR1s, int.post.density.stage1(p1, 1, r1, n1, alpha0, beta0, N))
        PoPr1s <- c(PoPr1s, 1-post.prob(alpha0, beta0, r1, n1, p0))
        PoPRs <- c(PoPRs, int.post.density(p1, 1, r1, r, n1, n, alpha0, beta0, N))
        PoPrs <- c(PoPrs, post.prob(alpha0, beta0, r, n, p1))
    }
    res.df <- cbind(PoPR1s, PoPr1s, PoPRs, PoPrs) %>% as.data.frame() %>% round(3)
    names(res.df) <- c("H1_R1", "H0_r1n1", "H1_R", "H1_rn")
    res.df <- cbind(tb, res.df)
    res.df[, c(5, 6, 1:4, 7:10)]
}


df1 <- list2df(ress[[1]]);df1
fsafda
df4 <- list2df(ress[[4]]);df4[c(1, 2, 4), ] 
df1 <- list2df(ress[[1]]);df1[c(1, 2, 4), ]
df2 <- list2df(ress[[2]]);df2[c(1, 2, 4), ]
df3 <- list2df(ress[[3]]);df3[c(1, 2, 4), ]
df4 <- list2df(ress[[4]]);df4
df1 <- list2df(ress[[1]]);df1
df2 <- list2df(ress[[2]]);df2
df3 <- list2df(ress[[3]]);df3

alpha0 <- 1
beta0 <- 1
N <- 50000
comb.res(ress[[4]], alpha0, beta0, N)[c(1, 2, 4), ]
comb.res(ress[[1]], alpha0, beta0, N)[c(1, 2, 4), ]
comb.res(ress[[2]], alpha0, beta0, N)[c(1, 2, 4), ]
comb.res(ress[[3]], alpha0, beta0, N)[c(1, 2, 4), ]


