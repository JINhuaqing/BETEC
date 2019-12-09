rm(list=ls())
setwd("C:/Users/Dell/Google Drive/multi-computers_folder/projects/BET_ext")
source("utilities.R")
library(magrittr)
library(dplyr)


Simon.df <- data.frame(
    r1=c(1, 2, 4, 8),
    n1=c(9, 12, 17, 22),
    r =c(3, 6, 11, 18),
    n =c(24, 35, 37, 46)
    )

BET.df <- data.frame(
    r1=c(1, 1, 3, 5),
    n1=c(7, 7, 10, 11),
    r =c(13, 16, 21, 24),
    n =c(37, 40, 41, 39)
)


bBET.df <- data.frame(
    r1=c(1, 2, 4, 5),
    n1=c(10, 11, 13, 12),
    r =c(26, 25, 20, 16),
    n =c(84, 67, 40, 25)
)

dbBET.df <- data.frame(
    r1=c(1, 1, 4, 7),
    n1=c(8, 6, 14, 18),
    r =c(14, 16, 22, 13),
    n =c(41, 41, 44, 38)
)

p0s <- c(0.05, 0.1, 0.2, 0.3)
p1s <- c(0.05, 0.1, 0.2, 0.3) + 0.2

combo <- function(res.df){
    res.df <- res.df %>% as.matrix()
    alpha0 <- 1
    beta0 <- 1
    N <- 50000
    PoPR1s <- c()
    PoPr1s <- c()
    PoPRs <- c()
    PoPrs <- c()
    esss0 <- c()
    esss1 <- c()
    pets0 <- c()
    pets1 <- c()
    for (i in 1:4){
        res.row <- res.df[i, ]
        r1 <- res.row[1]
        n1 <- res.row[2]
        r <- res.row[3]
        n <- res.row[4]
        p0 <- p0s[i]
        p1 <- p1s[i]
        PoPR1 <- int.post.density.stage1(p1, 1, r1, n1, alpha0, beta0, N)
        PoPr1 <- 1-post.prob(alpha0, beta0, r1, n1, p0)
        PoPR <- int.post.density(p1, 1, r1, r, n1, n, alpha0, beta0, N)
        PoPr <- post.prob(alpha0, beta0, r, n, p1)
        PoPR1s <- c(PoPR1s, PoPR1)
        PoPr1s <- c(PoPr1s, PoPr1)
        PoPRs <- c(PoPRs, PoPR)
        PoPrs <- c(PoPrs, PoPr)
        esss0 <- c(esss0, ESS(r1, n1, n, p0))
        esss1 <- c(esss1, ESS(r1, n1, n, p1))
        pets0 <- c(pets0, PET(r1, n1, p0))
        pets1 <- c(pets1, PET(r1, n1, p1))
    }
    res.df <- res.df %>% as.data.frame()
    res.df$PoPR1 <- PoPR1s 
    res.df$PoPr1 <- PoPr1s
    res.df$PoPR  <- PoPRs
    res.df$PoPr  <- PoPrs
    res.df$ESS0 <-  esss0
    res.df$ESS1 <-  esss1
    res.df$PET0 <-  pets0
    res.df$PET1 <-  pets1
    res.df
}


df.bb <- combo(bBET.df) %>% round(3)
df.db <- combo(dbBET.df) %>% round(3)
df.s <- combo(Simon.df) %>% round(3) 
df.b <- combo(BET.df)   %>% round(3)  

df.bb[, c(2, 1, 4, 3, 5:8, 11, 12, 9, 10)]
df.db[, c(2, 1, 4, 3, 5:8, 11, 12, 9, 10)]
df.b[, c(2, 1, 4, 3, 5:8, 11, 12, 9, 10)]
df.s[, c(2, 1, 4, 3, 5:8, 11, 12, 9, 10)]
