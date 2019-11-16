setwd("/Users/jinhuaqing/Downloads/inbox/BEText/")
library(magrittr)
library(dplyr)

# Load the data for delta method
load("ResultBoth5_25.RData")
res1 <- output.res
load("ResultBoth10_30.RData")
res2 <- output.res
load("ResultBoth20_40.RData")
res3 <- output.res
load("ResultBoth30_50.RData")
res4 <- output.res

res2df <- function(res){
    df1 <- lapply(res, function(x){do.call(rbind, x[[1]])}) %>% do.call(rbind, args=.)
    df2 <- lapply(res, function(x){do.call(rbind, x[[2]])}) %>% do.call(rbind, args=.)
    df.all <- cbind(df1, df2) %>% as.data.frame()
    names(df.all) <- c("r1", "n1", "r", "n", "a1", "a2")
    df.all
}

df1 <- res2df(res1$results)
df2 <- res2df(res2$results)
df3 <- res2df(res3$results)
df4 <- res2df(res4$results)

idx.discard <- (df1$n == 0) | (df2$n == 0) | (df3$n == 0) | (df4$n == 0)
idx.discard2 <- (df1$n >= 100) | (df2$n >= 100) | (df3$n >= 100) | (df4$n >= 100)
idx.discard <- idx.discard | idx.discard2
idx.keep <- !idx.discard

df1.res <- df1[idx.keep, ][, c(2, 4, 1, 3, 5, 6)]
df2.res <- df2[idx.keep, ][, c(2, 4, 1, 3, 5, 6)]
df3.res <- df3[idx.keep, ][, c(2, 4, 1, 3, 5, 6)]
df4.res <- df4[idx.keep, ][, c(2, 4, 1, 3, 5, 6)]

idx.man <- c(2, 23, 41)
df1.res[idx.man, ]
df2.res[idx.man, ]
df3.res[idx.man, ]
df4.res[idx.man, ]
