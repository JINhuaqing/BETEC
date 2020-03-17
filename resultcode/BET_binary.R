rm(list=ls())
setwd("C:/Users/Dell/Google Drive/multi-computers_folder/projects/bBET")
library(magrittr)
source("utilities.R")
library(TeachingDemos)
 
 
betabn<-function(x,n,a,b){choose(n,x)*beta(a+x,b+n-x)/beta(a,b)}

hpdresult<-function(x,n,a,b,Pi){
h=hpd(qbeta, shape1=a+x, shape2=b+n-x,conf=Pi)
return(h[2]-h[1])
}


poptestvalue<-function(x1,n1,xpr1,xpr2,ps){
return(1-pbeta(ps,xpr1+x1,xpr2+n1-x1))
}

findr<-function(n,Pi,a,b,ps){
#minimum number of response required to achieve the PoP >0.9 criterion

temp<-function(x){poptestvalue(x,n,a,b,ps)-Pi}

if (temp(0)>0){return (0)}
else if(temp(0)<0 && temp(n)<0){
return(999)
}
else {

y2root=as.integer(uniroot(temp,c(0,n))$root)
	while(temp(y2root)<0){y2root=y2root+1}
	
	return (y2root)
	
}
}


findparam0<-function(n,Pi,a,b,ps){
r=findr(n,Pi,a,b,ps)
l=hpdresult(r,n,a,b,Pi)
return(c(r,l))
}


findparam<-function(Pi,a,b,ps,target,maxn=200,minn=10){

temp<-function(n){
	return(findparam0(n,Pi,a,b,ps)[2]-target)
}


if (is.na(temp(minn)))
{
	found=FALSE
	while(found==FALSE)
	{
		minn=minn+1
		if (!(is.na(temp(minn)) || temp(minn)>0))
			found=TRUE
		
		if (minn>maxn){
			print("Error in the parameter setting maxn and minn")
			return(0)
		}
	}
	r=findr(minn,Pi,a,b,ps)
	return(list(n=minn,r=r))
}
else if (temp(minn)<0)
{
	r=findr(minn,Pi,a,b,ps)
	return(list(n=minn,r=r))
}

 if(temp(minn)<0 && temp(maxn)<0){
	while(temp(minn)<0 && minn>0){minn=minn-1}
	y2root=minn+1
	r=findr(y2root,Pi,a,b,ps)
	return (list(n=y2root,r=r))
}else if(temp(minn)>0 && temp(maxn)>0){
	while(temp(maxn)>0){maxn=maxn+100}
}




y2root=as.integer(uniroot(temp,c(minn,maxn))$root)
	while(temp(y2root)>0){y2root=y2root+1}


r=findr(y2root,Pi,a,b,ps)
	return (list(n=y2root,r=r))
	
}



p0=0.2               ######### The uninteresting response rate in the null hypothesis
p1=0.35               ######### The desirable target response rate in the alternative hypothesis
l1=0.25               ######### Required HPD interval length for sta
l2=0.21              ######### Required HPD interval length for stage 2
Pi1=0.9          ######### Posterior probability cutoff for stage 1
Pi2=0.9          ######### Posterior probability cutoff for stage 2
alpha0 = 1
beta0 = 1
minn=1               ######### Minimum sample size for the first stage; 
                     ######### for noninformative prior, typically set as 1; for informative prior typically set as 10 to 15

					 
					 
stage1nr=findparam(Pi1,alpha0,beta0,p0,l1,200,minn)
stage2nr=findparam(Pi2,alpha0,beta0,p1,l2,200,minn)
stage1nr
stage2nr

r1 <- stage1nr$r
n1 <- stage1nr$n
r <- stage2nr$r
n <- stage2nr$n
#r1 <- 7
#n1 <- 33
#r <- 16
#n <- 58
N <- 50000
PoPR1s <- int.post.density.stage1(p1, 1, r1, n1, alpha0, beta0, N)
PoPr1s <- 1-post.prob(alpha0, beta0, r1, n1, p0)
PoPRs <- int.post.density(p1, 1, r1, r, n1, n, alpha0, beta0, N)
PoPrs <- post.prob(alpha0, beta0, r, n, p1)
c(PoPR1s, PoPr1s, PoPRs, PoPrs) %>% round(3)
