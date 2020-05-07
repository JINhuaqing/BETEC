source("utilities.R")
#load("BETECf3.Rdata")
##load("BETECp2.Rdata")
#ls()
#
#for (i in 1:12)
#{
#    res <- ress[[i]]
#    if (res$p01[1]==0.3)
#    print(c(res$a12, res$p01, res$res))
#   }
#
p0 <- 0.3
p1 <- 0.5
res <- c(2, 7, 8, 15)
stats1 <- round(Combo.Results(res, p0, p1, N=100000), 6)[c(11, 9, 6,  7, 4, 5, 1, 2, 3)]
stats2 <- round(Combo.Results(res, p0, p1, N=100000), 10)[c(11, 9, 6,  7, 4, 5, 1, 2, 3)]
c(res, stats1)
c(res, stats2)

