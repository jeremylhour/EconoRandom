### Illustration du theoreme de Frisch-Waughn
### Jeremy L Hour
### 9 mars 2016

rm(list=ls())
library("MASS")
library("AER")


### A. Simulation
n <- 1000
p <- 6

### Covariate correlation coefficients
rho <- .5
Sigma <- matrix(0,nrow=p, ncol=p)

for(k in 1:p){
  for(j in 1:p){
    Sigma[k,j] <- rho^abs(k-j)
  }
}

# Simulate covariates
X <- mvrnorm(n = n, mu=rep(0,p), Sigma)

# Coefficients
gamma <- rep(0,p)

for(j in 1:abs(p/2)){
  gamma[j] <- 1*(-1)^(j) / j^2
}


# Simulate covariates
X <- mvrnorm(n = n, mu=rep(0,p), Sigma)

# Simulate outcome
y <- X%*%gamma + rnorm(n)

### 1. linear regression
linreg <- lm(y ~ X)
summary(linreg)

### 2. two step estimator
step_one <- lm(X[,1] ~ X[,-1])
eta <- step_one$residuals
step_two <- lm(y ~ eta)
summary(step_two)

### Conclusion: les coef sont les memes
print(coef(linreg)["X1"])
print(coef(step_two)["eta"])


### B. Instrumental variable regression
data("CigarettesSW")
CigarettesSW$rprice <- with(CigarettesSW, price/cpi)
CigarettesSW$rincome <- with(CigarettesSW, income/population/cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax)/cpi)

### 1. 2SLS
IVstep_one <- lm(log(rprice) ~ log(rincome) + tdiff + I(tax/cpi), data = CigarettesSW)
summary(IVstep_one)
price_hat <- predict(IVstep_one)

IVstep_two <- lm(log(packs) ~ price_hat + log(rincome), data = CigarettesSW)
summary(IVstep_two)

### 2. Control Function Approach
CFstep_one <- lm(log(rprice) ~ log(rincome) + tdiff + I(tax/cpi), data = CigarettesSW)
summary(CFstep_one)
eta_cig <- CFstep_one$residuals

CFstep_two <- lm(log(packs) ~ log(rprice) + log(rincome) + eta_cig, data = CigarettesSW)
summary(CFstep_two)

### Conclusion: coefficients associes au prix sont les memes
print(coef(IVstep_two)["price_hat"])
print(coef(CFstep_two)["log(rprice)"])