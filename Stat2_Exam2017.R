### Statistics 2 - Examen 2017
### Jeremy L Hour

### Tirer des variables selon la densite
rTwoUnif <- function(n=100,theta=.5){
  u = rbinom(n,1,prob=(1+theta)/2)
  X = runif(n,min=.5,max=1) - .5*u
  return(X)
}

### log-likelihood
LLTU <- function(theta,data){
  n = length(data)
  n1 = sum(data<.5)
  return(log(1+theta)*n1/n + log(1-theta)*(1-n1/n) )
}


data = rTwoUnif(100,theta=1/3)
### Analytical MV
n1 = sum(data<.5)
theta_MV = 2*(n1/length(data)) - 1

### Plot
theta.set = seq(-1,1,by=.01)
plot(theta.set,LLTU(theta.set,data), type="l")
abline(v=theta_MV,lty=2,lwd=2,col="red")