library(magrittr)
post.f <- function(r, n){
  ff <- function(x){dbeta(x, 1+r, 1+n-r)}
  ff
}

setwd("C:/Users/Dell/Documents/ProjectCode/BETEC/resultcode")

# BETEC
f1 <- post.f(19, 75)
f2 <- post.f(17, 56)
f3 <- post.f(16, 39)
f4 <- post.f(13, 25)
jpeg(file="BETEC.jpg")
curve(f1, 0, 1, lty=1, col="red", lwd=2, xlab="p", ylab="Density", xaxt="n", ylim=c(0, 8))
curve(f2, 0, 1, lty=2, col="blue", lwd=2, add=TRUE)
curve(f3, 0, 1, lty=3, col="green", lwd=2, add=TRUE)
curve(f4, 0, 1, lty=4, col="purple", lwd=2, add=TRUE)

ex1 <- expression(paste(p[0]==0.05, ", ", p[1]==0.25))
ex2 <- expression(paste(p[0]==0.1, ", ", p[1]==0.3))
ex3 <- expression(paste(p[0]==0.2, ", ", p[1]==0.4))
ex4 <- expression(paste(p[0]==0.3, ", ", p[1]==0.5))
axis(1, seq(0, 1, 0.05), seq(0, 1, 0.05))
text(1, 8, labels="(A)")
#legend("topright", c(ex1, ex2, ex3, ex4), lty=1:4, col=c("red", "blue", "green", "purple"))
dev.off()


# delta-BETEC
f1 <- post.f(13, 51)
f2 <- post.f(17, 56)
f3 <- post.f(24, 59)
f4 <- post.f(28, 55)

jpeg(file="delta-BETEC.jpg")
curve(f1, 0, 1, lty=1, col="red", lwd=2, xlab="p", ylab="Density", xaxt="n", ylim=c(0, 8))
curve(f2, 0, 1, lty=2, col="blue", lwd=2, add=TRUE)
curve(f3, 0, 1, lty=3, col="green", lwd=2, add=TRUE)
curve(f4, 0, 1, lty=4, col="purple", lwd=2, add=TRUE)

ex1 <- expression(paste(p[0]==0.05, ", ", p[1]==0.25))
ex2 <- expression(paste(p[0]==0.1, ", ", p[1]==0.3))
ex3 <- expression(paste(p[0]==0.2, ", ", p[1]==0.4))
ex4 <- expression(paste(p[0]==0.3, ", ", p[1]==0.5))
axis(1, seq(0, 1, 0.05), seq(0, 1, 0.05))
text(1, 8, labels="(B)")
#legend("topright", c(ex1, ex2, ex3, ex4), lty=1:4, col=c("red", "blue", "green", "purple"))
dev.off()
