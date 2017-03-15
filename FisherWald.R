### Equivalence Wald-Fisher

### AUtre
x = seq(0,5,by=.01)


n = 1000000
y1 = df(x,5,n)
y2 = dchisq(x*5,5)*5


plot(x, y1, col="red", lwd=1,type="line")
lines(x, y2, col="blue", lwd=1)


### Verifions
library("MASS")
p = 7
n = 100
rho = .5

### Covariate variance matrix
Sigma = matrix(0,nrow=p, ncol=p)

for(k in 1:p){
  for(j in 1:p){
    Sigma[k,j] = rho^abs(k-j)
  }
}


### Outcome equation coefficients
b = rep(0,p)

for(j in 1:p){
  b[j] <- (-1)^(j+1) / (p-j+1)^2
}

Ry=.3
c = sqrt((1/t(b)%*%Sigma%*%b)*(Ry/(1-Ry)))
b = c*b

X = mvrnorm(n = n, mu=rep(0,p), Sigma)
y = 1 + X%*%b + rnorm(n)

X = cbind(rep(1,n),X)

b_OLS = solve(t(X)%*%X) %*% (t(X)%*%y)

### Fisher
eps_UC = y - X%*%b_OLS
eps_C = y - mean(y)

Fisher = ((n-p-1)/p) * (sum(eps_C^2) - sum(eps_UC^2))  / sum(eps_UC^2)

### Wald
VCov = solve(t(X)%*%X)*sum(eps_UC^2)/(n-p-1)
Wald = t(b_OLS[-1]) %*% solve(VCov[2:(p+1),2:(p+1)]) %*% b_OLS[-1]

summary(lm(y ~ X[,-1]))