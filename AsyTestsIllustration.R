### Illsutration Tests Asymptotiques
### Jeremy L Hour
### 20 septembre 2016


rm(list=ls())
set.seed(12071990)


### 0. Settings

### Load packages
library("MASS")
library("ggplot2")
library("gridExtra")

### Pour un param. theta quelconque
### on veut tester:
### H_0: theta >= 0 vs. H_1: theta < 0

pi = function(theta,n,sigma=1){
  pnorm(qnorm(.05) - sqrt(n)*theta/sigma)
}

# pi est la function de puissance "asymptotique"
x = seq(-5,5,by=.01)
plot(x,pi(x,1),type="line", col="steelblue",lwd=2)
abline(v=0, lty=3)
abline(h=.05, lty=3)
