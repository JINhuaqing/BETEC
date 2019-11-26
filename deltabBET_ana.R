setwd("/Users/jinhuaqing/Downloads/inbox/BEText/")
setwd("C:/Users/Dell/Downloads/inbox/BEText")
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

list2df(ress[[4]])[c(1, 5, 6), ]
list2df(ress[[1]])[c(1, 5, 6), ]
list2df(ress[[2]])[c(1, 5, 6), ]
list2df(ress[[3]])[c(1, 5, 6), ]

# compute the PoP of H_0, H_1

