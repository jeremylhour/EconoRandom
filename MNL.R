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
set.seed(12071990)
data("mtcars")
J <- nrow(mtcars)

# We'll have 4 regressors: Miles per gallon, 
# weight, Transmission type and price
X <- data.frame("Intercept"=rep(1,J),
                 mtcars[,c("mpg","am","wt")])

# Define the prices
Z <- exp( mtcars[,"disp"]/200 + mtcars[,"carb"]/30 + .005*rnorm(J) )
eta <- exp(rnorm(J)) #Brand image
price <- exp( 1 + mtcars[,"mpg"]/80 + mtcars[,"wt"]/8 - mtcars[,"am"] + 5*log(Z) + 3*eta + rnorm(J)) 

price/Z #mark-up
X[,"log.price"] <- log(price)
X[,"log.cost"] <- log(Z)

linmod <- lm(log.price ~ mpg + wt + am + log.cost, data=X)
summary(linmod)

# define coefficients in terms of WTP
WTP <- c(0,750,-200,500) 
beta_price <- -.005
beta <- c(-WTP,1)*beta_price

# Form non-random part of utility X_j%*%beta + xi_j
util <- as.matrix(X[,c("Intercept","mpg","am","wt","log.price")])%*%beta + 5* eta + rnorm(J, sd=50)
LM <- lm(util ~ as.matrix(X[,c("Intercept","mpg","am","wt","log.price")]) - 1) 
summary(LM)
-coef(LM)/coef(LM)[5]

# Simulate consumer's decisions
n <- 10000
eps <- matrix(-log(-log(runif(n*J))), nrow=n, ncol=J) # Simulate type I GEV
totutil <- rep(1,n) %x% t(util/sd(util)) + eps 
  
y <- mapply(function(x) which(totutil[x,]==max(totutil[x,])),1:n)
y <- y*apply(totutil,1,function(x) any(x>0)) # if negative utility, don't buy a car

# Some people are not buyers
y[sample(1:n,100)] <- 0

marketshare <- data.frame("Car"=c("None",rownames(mtcars)),
                          "MS"=as.numeric(100*table(y)/n) )

print(marketshare)

### 2. ML Estimation
MNLlogLik(beta/sd(util),y,X[,c("Intercept","mpg","am","wt","log.price")])

# Optimization
par0 <- rep(0,5)
MNLfit <- maxBFGS(MNLlogLik, start=par0, print.level=4, finalHessian=T, y=y, X=X[,c("Intercept","mpg","am","wt","log.price")] )
summary(MNLfit)

-MNLfit$estimate/MNLfit$estimate[5]

### 3. Regression analysis
s <- as.vector(table(y)/n)
y_tilde <- log(s) - log(s[1])
y_tilde <- y_tilde[-1]

Regfit <- lm(y_tilde ~ mpg + am + wt + log.price,data=X)
summary(Regfit)

plot(beta,MNLfit$estimate)
points(beta,coef(Regfit))

# Display coefficients in terms of WTP (in dollars)
-coef(Regfit)/coef(Regfit)[5]

### 4. Instrumental variable regression
FSReg <- lm(log.price ~ mpg + am + wt + log.cost,data=X)
summary(FSReg)

IVReg <- lm(y_tilde ~ mpg + am + wt + predict(FSReg),data=X)
summary(IVReg)
-coef(IVReg)/coef(IVReg)[5]