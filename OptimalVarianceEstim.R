### Optimal variance estimator
### 21/02/2017
### Jeremy L'Hour


### Initialisation
n = 100
R = 100000
lambda = 2
var = 1/lambda^2

Shat = function(x) sum((x-mean(x))^2)

### Monte Carlo
EstimCollect = matrix(, ncol=3, nrow=R)
for(r in 1:R){
  sample = rexp(n,rate=lambda)
  EstimCollect[r,] = Shat(sample)*c(1/(n-1),1/n,1/(n+1))
}

### Bias
print("Bias")
print(abs(apply(EstimCollect,2,mean)-var))

### MSE
print("Mean-squared Error")
print(apply((EstimCollect-var)^2,2,mean))