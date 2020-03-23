## The BETEC design and delta-BETEC design for phase II single-arm clinical trials with binary endpoints.

The functions defined in utilities.R file of formalcode folder

```R
 alpha0 <- 1
 beta0 <- 1

 
 # Test probabilities
 p0 <- 0.3
 p1 <- 0.5
 
 # 4 hyper-parameters
 pi1 <- 0.8
 pi2 <- 0.9
 
 a1 <- 0.1
 a2 <- 0.25
 b1 <- 0.05
 b2 <- 0.05
 
 res.BETEC <- BETEC(p0, p1, pi1, pi2, a1, a2, alpha0=1, beta0=1, nMax=200, N=10000)
 res.deltaBETEC <- deltaBETEC(p0, p1, pi1, pi2, b1, b2)
```


This repository also includes 

- the CPP code for simon's design (Simon, 1989) with early stopping for efficacy. 

- The R code for BET design (Shi and Yin, 2018).

- The R code for STD and DTD design (Tan and Machin, 2002).
