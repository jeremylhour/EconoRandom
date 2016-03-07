### Multinomial Logit Model
### Jeremy L Hour
### 7 mars 2016

### Set working directory
setwd("//ulysse/users/JL.HOUR/1A_These/EconoRandom")

rm(list=ls())


### 0. Settings
### Load packages
library(maxLik)
library(flexsurv)

### Load user-defined functions
source("functions/MNLlogLik.R")


### 1. Load data: simulate a choice model
data("mtcars")
J <- nrow(mtcars)
p <- ncol(mtcars)

# Form non-random part of utility X_j%*%beta + xi_j
set.seed(12071990)
beta <- c(1,2,.2,.3,1,1,1,.9,-3,1,3)
util <- as.matrix(mtcars)%*%beta + rnorm(J, sd=1)
utilcut <- quantile(util,.2)

# Simulate consumer's decisions
n <- 10000
eps <- matrix(rgompertz(n*J, shape=1, rate = 1), nrow=n, ncol=J)
totutil <- rep(t(util),n) + eps - utilcut
  
y <- mapply(function(x) which(totutil[x,]==max(totutil[x,])),1:n)
y <- y*apply(totutil,1,function(x) all(x>0)) # if negative utility, don't buy a car

100*table(y)/n # display simulated market share

### 2. ML Estimation
MNLlogLik(beta,y,mtcars)


# Optimization
par0 <- rep(0,p+1)
MNLfit <- maxBFGS(MNLlogLik, start=par0, print.level=4, finalHessian=T, y=y, X=cbind(rep(1,J),as.matrix(mtcars)) )

logLik(MNLfit$estimate,y,cbind(rep(1,J),as.matrix(mtcars)))
MNLfit$estimate
plot(beta,MNLfit$estimate[-1])

### 3. Regression analysis
s <- as.vector(table(y)/n)
y_tilde <- log(s) - log(s[1])
y_tilde <- y_tilde[-1]

Regfit <- lm(y_tilde ~ .,data=mtcars)