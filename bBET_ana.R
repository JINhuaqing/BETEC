setwd("/Users/jinhuaqing/Downloads/inbox/BEText/")
setwd("C:/Users/Dell/Downloads/inbox/BEText")
library(magrittr)
library(dplyr)

# Load the data for delta method
fs <- list.files(pattern="bBET.*.RData")
ress <- list()
for (i in 1:length(fs)){
    load(fs[i])
    ress[[i]] <- output.res
}

res2df <- function(res){
    df1 <- lapply(res, function(x){do.call(rbind, x[[1]])}) %>% do.call(rbind, args=.)
    df2 <- lapply(res, function(x){do.call(rbind, x[[2]])}) %>% do.call(rbind, args=.)
    df.all <- cbind(df1, df2) %>% as.data.frame()
    names(df.all) <- c("r1", "n1", "r", "n", "a1", "a2")
    df.all
}

df4 <- res2df(ress[[4]]$results);df4[c(1, 2, 4), ] 
df1 <- res2df(ress[[1]]$results);df1[c(1, 2, 4), ]
df2 <- res2df(ress[[2]]$results);df2[c(1, 2, 4), ]
df3 <- res2df(ress[[3]]$results);df3[c(1, 2, 4), ]

