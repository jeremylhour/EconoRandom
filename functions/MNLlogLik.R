#' Conditional Logit Log-Likelihood
#' 
#' Last edited: 7 mars 2016
#' 
#' @param param vector of parameters
#' @param y Outcome taking integer values from 0 to J
#' @param X Matrix of covariates
#' 
#' @return Returns value of log-likelihood
#' 
#' @author Jeremy Lhour


MNLlogLik <- function(param,y,X){
  # Compute choice probabilities
  C <- sum(exp(as.matrix(X)%*%param))+1
  lprob <- as.matrix(X)%*%param - log(C)
  lprob <- as.vector(lprob)
  
  # Select contribution to likelihood
  ll <- ifelse(y==0,-log(C),lprob[y])
  return(sum(ll))
}