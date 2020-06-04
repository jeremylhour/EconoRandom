### DGP

import numpy as np
from scipy.stats import norm

def data_sim(n=2000,p=50,Ry=.5,Rd=.2,rho=.5,tau=0):
    Sigma = [] # variance-covariance matrix
    for i in range(0,p):
        new_line = [rho**abs(i-j) for j in range(0,p)]
        Sigma.append(new_line)
    
    gamma = [(-1)**i / i**2 for i in range(1,int(abs(p/2))+1)]
    gamma = np.pad(gamma, (0,int(abs(p/2))))
    
    b = [*gamma[0:int(abs(p/2))], *[(-1)**(i+1) / (p-i+1)**2 for i in range(int(abs(p/2)),p)]]
    
    gamma, b = np.array(gamma), np.array(b)
    
    gamma = np.sqrt((1/gamma.dot(Sigma).dot(gamma))*(Rd/(1-Rd)))*gamma
    b = np.sqrt((1/b.dot(Sigma).dot(b))*(Ry/(1-Ry)))*b
    
    # Simulate data
    X = np.random.multivariate_normal([0]*p, Sigma, n)
    d = norm.cdf(X.dot(gamma))



    
    
    np.random.multivariate_normal(mean, cov[, size, check_valid, tol])¶
    

  
  
  X <- mvrnorm(n = n, mu=rep(0,p), Sigma)
  d <- as.numeric(runif(n) < pnorm(X%*%gamma))
  
  ### Treatment effect
  a <- 0
  if(TreatHeter) a <- X%*%rep(10,p)
  a <- a - sum(d*a)/sum(d)

  y <- a*d + X%*%b + rnorm(n)

  if(Intercept) X <- cbind(rep(1,n),X)
  
  return(list(X=X,
              y=y,
              d=d,
              b=b,
              g=gamma,
              ATT=sum(d*a)/sum(d)))
}