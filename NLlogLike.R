### Nested logit with individual-specific variables

NLlogLik <- function(par,y,X){
  ### Log-likelihood for nested Logit with:
  ### y=3 is the normalized alternative
  ### y=2 and y=3 are nested
  ### X is a n times p matrix of individual-specific regressors
  
  
  n <- nrow(X)
  p <- ncol(X)
  
  # Assigning parameters
  g1 <- par[1:p] # coefficient for alternative one
  g2 <- par[(p+1):(2*p)] # coefficient for alternative two
  sigma <- 1/(1+exp(-par[2*p+1])) # Transformation to 
  
  # Computing denominator
  denom <- exp((1-sigma)*(X%*%g1)) + (1+exp(X%*%g2))^(1-sigma)
  ldenom <- log(denom)
  
  # Computing numerator
  lNum <- matrix(ncol=3, nrow=n)
  
  lNum[,1] <- (1-sigma)*(X%*%g1)
  lNum[,2] <- X%*%g2 - sigma*log(1+exp(X%*%g2))
  lNum[,3] <- - sigma*log(1+exp(X%*%g2))
  
  # Contrib
  Contrib <- mapply(function(x) lNum[x,y[x]]-ldenom[x],1:n)
  
  return(sum(Contrib))
}

## Datasimu
p <- 2
n <- 10000
X <- matrix(rnorm(n*p), ncol=p)

y <- rmultinom(n, 2, prob=c(1/3,1/3,1/3))[1,] + 1

par0 <- c(rep(1,4),0)

library(maxLik)
MLE <- maxBFGS(NLlogLik, start=par0, print.level=4, finalHessian=T, y=y,X=X)
summary(MLE)

# Obtain desired coefficients:
sigma <- 1/(1+exp(-MLE$estimate[2*p+1]))
g1 <- (1-sigma)*MLE$estimate[1:p] # coefficient for alternative one
g2 <- (1-sigma)*MLE$estimate[(p+1):(2*p)]

# Delta method to get startard error