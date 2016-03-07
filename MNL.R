### Conditional Logit Model
### Jeremy L Hour
### 7 mars 2016

### Set working directory
setwd("//ulysse/users/JL.HOUR/1A_These/EconoRandom")

rm(list=ls())


### 0. Settings
### Load packages
library(maxLik)

### Load user-defined functions
source("functions/MNLlogLik.R")


### 1. Load data: simulate a choice model
data("mtcars")
J <- nrow(mtcars)
p <- ncol(mtcars)

# Form non-random part of utility X_j%*%beta + xi_j
set.seed(12071990)
beta <- .1*rnorm(11)/apply(mtcars,2,sd)
util <- as.matrix(mtcars)%*%beta + rnorm(J, sd=1)

# Simulate consumer's decisions
n <- 10000
eps <- matrix(-log(-log(runif(n*J))), nrow=n, ncol=J) # Simulate type I GEV
totutil <- rep(1,n) %x% t(util) + eps 
  
y <- mapply(function(x) which(totutil[x,]==max(totutil[x,])),1:n)
y <- y*apply(totutil,1,function(x) any(x>0)) # if negative utility, don't buy a car

marketshare <- data.frame("Car"=c("None",rownames(mtcars)),
                          "MS"=as.numeric(100*table(y)/n) )

print(marketshare)

### 2. ML Estimation
MNLlogLik(beta,y,mtcars)

# Optimization
par0 <- rep(0,p+1)
MNLfit <- maxBFGS(MNLlogLik, start=par0, print.level=4, finalHessian=T, y=y, X=cbind(rep(1,J),mtcars) )
summary(MNLfit)


### 3. Regression analysis
s <- as.vector(table(y)/n)
y_tilde <- log(s) - log(s[1])
y_tilde <- y_tilde[-1]

Regfit <- lm(y_tilde ~ .,data=mtcars)
summary(Regfit)

plot(beta,MNLfit$estimate[-1])
points(beta,coef(Regfit)[-1])
