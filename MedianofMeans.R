### Median of Means
### Jeremy L Hour
### 25/10/2017

### Set working directory
setwd("//ulysse/users/JL.HOUR/1A_These/B. ENSAE Classes/EconoRandom")

rm(list=ls())
set.seed(12071990)

### Settings
R = 10000
n = 100
eps = .1 #proportion of contaminated sample
K = trunc(log(n))
nc = trunc(eps*n)
nnc = n-nc
  
### 1. Estimating a mean
Result = matrix(nrow=R,ncol=2)
for(r in 1:R){
  data = c(rnorm(nnc),rcauchy(nc))
  uu=0 
  while(uu==0){
    allocation = sample(1:K,n,replace=T)
    uu=min(mapply(function(x) sum(allocation==x),1:K))
  }
  Result[r,] = c(mean(data), median(mapply(function(x) mean(data[allocation==x]),1:K)) )
  print(c(mean(data), median(mapply(function(x) mean(data[allocation==x]),1:K)) ))
}

print("MSE:")
print(apply(Result^2,2,mean))

### 2. Estimating a regression coefficient...
Result = matrix(nrow=R,ncol=2)
for(r in 1:R){
  data = c(rnorm(nnc),rcauchy(nc))
  uu=0 
  while(uu==0){
    allocation = sample(1:K,n,replace=T)
    uu=min(mapply(function(x) sum(allocation==x),1:K))
  }
  Result[r,] = c(mean(data), median(mapply(function(x) mean(data[allocation==x]),1:K)) )
  print(c(mean(data), median(mapply(function(x) mean(data[allocation==x]),1:K)) ))
}

print("MSE:")
print(apply(Result^2,2,mean))