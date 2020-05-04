load("BETEC.Rdata")
ls()

for (i in 1:12)
{
    res <- ress[[i]]
    print(c(res$a12, res$p01, res$res))
   }
