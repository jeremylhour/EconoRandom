### Stein's Unbiased Risk Estimate and Stein's Paradox
### Jeremy L Hour
### 30/10/2017

### Set working directory
setwd("//ulysse/users/JL.HOUR/1A_These/B. ENSAE Classes/EconoRandom")

rm(list=ls())
set.seed(12071990)

### Packages
library("MASS")


### Functions
JSestim = function(X){
  d = length(X)
  return((1-(d-2)/sum(X^2))*X)
}

PosJSestim = function(X){
  d = length(X)
  if(1 > (d-2)/sum(X^2)){
    mu = JSestim(X)
  } else {
    mu = rep(0,d)
  }
  return(mu)
}

### 1. Illustration of Stein's paradox
d = 10
R = 1000
theta = rep(0,d)
for(j in 1:d){
  theta[j] = .9^j
}

# Start the loop
Results = matrix(nrow=R, ncol=3)
for(r in 1:R){
  X = mvrnorm(n = 1, theta, Sigma=diag(d))
  R1 = sum((X-theta)^2)
  R2 = sum((JSestim(X)-theta)^2)
  R3 = sum((PosJSestim(X)-theta)^2)
  Results[r,] = c(R1,R2,R3)
}

print("Results")
apply(Results,2,mean)

### 2. RCT Regression setup
tau = 0
N = 100
Results = matrix(nrow=R, ncol=3)
for(r in 1:R){
  X = mvrnorm(n = N, theta, Sigma=diag(d))
  eps = rnorm(n = N)
  y = X%*%theta + eps
  
  bOLS = solve(t(X)%*%X) %*% (t(X)%*%y)
  bJS = JSestim(bOLS)
  bposJS = PosJSestim(bOLS)
  
  D = rbinom(N,size=1,.2)
  
  tauOLS = t(D)%*%(y-X%*%bOLS) / sum(D)
  tauJS = t(D)%*%(y-X%*%bJS) / sum(D)
  tauposJS = t(D)%*%(y-X%*%bposJS) / sum(D)

  Results[r,] = c(tauOLS^2,tauJS^2,tauposJS^2)
}

print("Results")
apply(Results,2,mean)
