### Panel Models with Interactive Fixed Effects (Bai, 2009)
### J LHour
### 11 mars 2016

### Set working directory
setwd("//ulysse/users/JL.HOUR/1A_These/EconoRandom")

rm(list=ls())

### Load packages
library("plm")

### Load data
wagepanel <- read.table("data/WAGEPAN.raw", header = F)
colnames(wagepanel) <- c("nr","year","black","exper","hisp","hours","married","occ1",     
                         "occ2","occ3","occ4","occ5","occ6","occ7","occ8","occ9",     
                         "educ","union","lwage","d81","d82","d83","d84","d85",      
                         "d86","d87","expersq")
wagepanel[,"d80"] <- as.numeric(wagepanel[,"year"]==1980)
wagepanel <- wagepanel[order(wagepanel[,"nr"]),]
indiv <- unique(wagepanel[,"nr"])
time <- unique(wagepanel[,"year"])

n <- length(indiv)
t0 <- length(time)

### Bai (2009) estimator

r <- 3 # Number of factors

# Initial estimator : Simple panel regression
WLS <- plm(lwage ~ exper + expersq, data=wagepanel,
            effect="individual",model="within",index=c("nr","year")) 
summary(WLS)

# Starting iteration
beta <- coef(WLS)
X <- as.matrix(wagepanel[,c("exper","expersq")])

for(i in 1:3){
  
  print(beta)
  
  eps <- wagepanel[,"lwage"] - X%*%beta
  U <- t(matrix(eps, ncol=n))
  W <- t(U)%*%U / n
  
  Factor <- eigen(W)$vector[,1:r]
  Lambda <- t(Factor) %*% t(U)/t0
  
  M <- diag(n) %x% (diag(t0) - Factor%*%t(Factor)/t0)
  beta <- solve(t(X)%*% X) %*% (t(X) %*% (wagepanel[,"lwage"] - rep(apply(Factor %*% Lambda,2,sum),each=t0)))
  
}

yhat <- X%*%beta + rep(apply(Factor %*% Lambda,2,sum),each=t0)
plot(wagepanel[,"lwage"],yhat)

hist(Lambda[3,])