setwd("/Users/jinhuaqing/Downloads/inbox/BEText/")
library(magrittr)
library(dplyr)

# Load the data for delta method
load("ResultDelta5_25.RData")
res1 <- output.res
load("ResultDelta10_30.RData")
res2 <- output.res
load("ResultDelta20_40.RData")
res3 <- output.res
load("ResultDelta30_50.RData")
res4 <- output.res


res1.res <- do.call(rbind, res1$result) %>% as.data.frame()
res1.para <- do.call(rbind, res1$corparas) %>% as.data.frame()
#filter(res1.para,
#       (30>=res1.res$n1) & (res1.res$n1>=4) & (res1.res$r > res1.res$n1) & (res1.res$n>=15) & (res1.res$n <=70))
res.idx1 <- filter(data.frame(idx=1:400),
       (30>=res1.res$n1) & (res1.res$n1>=5) & (res1.res$r > res1.res$n1) & (res1.res$n>=20) & (res1.res$n <=70))

res2.res <- do.call(rbind, res2$result) %>% as.data.frame()
res2.para <- do.call(rbind, res2$corparas) %>% as.data.frame()
res.idx2 <- filter(data.frame(idx=1:400),
       (30>=res2.res$n1) & (res2.res$n1>=6) & (res2.res$r > res2.res$n1) & (res2.res$n>=20) & (res2.res$n <=70))

res3.res <- do.call(rbind, res3$result) %>% as.data.frame()
res3.para <- do.call(rbind, res3$corparas) %>% as.data.frame()
res.idx3 <- filter(data.frame(idx=1:400),
       (30>=res3.res$n1) & (res3.res$n1>=6) & (res3.res$r > res3.res$n1) & (res3.res$n>=20) & (res3.res$n <=70))

res4.res <- do.call(rbind, res4$result) %>% as.data.frame()
res4.para <- do.call(rbind, res4$corparas) %>% as.data.frame()
res.idx4 <- filter(data.frame(idx=1:400),
       (30>=res4.res$n1) & (res4.res$n1>=6) & (res4.res$r > res4.res$n1) & (res4.res$n>=20) & (res4.res$n <=70))

idx.keep <- res.idx1 %>% intersect(res.idx2) %>% intersect(res.idx3) %>% intersect(res.idx4)
idx.keep

res3.para[idx.keep$idx, ]
res3.res[idx.keep$idx, ]


# find the a1 a2 manully.
res1.para
idx.wanted <- c(83, 85, 127, 190)
res1.para[idx.wanted, ]
res2.para[idx.wanted, ]
res3.para[idx.wanted, ]
res4.para[idx.wanted, ]

res1.res[idx.wanted, ][, c(2, 4, 1, 3)]
res2.res[idx.wanted, ][, c(2, 4, 1, 3)]
res3.res[idx.wanted, ][, c(2, 4, 1, 3)]
res4.res[idx.wanted, ][, c(2, 4, 1, 3)]
