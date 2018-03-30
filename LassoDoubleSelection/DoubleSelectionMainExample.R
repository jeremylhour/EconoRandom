### Double-selection
### Jeremy L Hour
### 26 juillet 2017

### Set working directory
setwd("//ulysse/users/JL.HOUR/1A_These/B. ENSAE Classes/EconoRandom/LassoDoubleSelection")
rm(list=ls())


### 0. Settings

### Load packages
library("MASS")
library("reshape2")
library("ggplot2")
library("gridExtra")

### Load user-defined functions
source("MainDGP.R") 
source("//ulysse/users/JL.HOUR/1A_These/LassoFISTA/functions/LassoFISTA.R")
source("//ulysse/users/JL.HOUR/1A_These/LassoFISTA/functions/RidgeFISTA.R")


### 1. Simulate data
R = 1000
n = 500
p = 1000

c = 1.1
g = .1/log(max(p,n))
lambda = c*qnorm(1-.5*g/p)/sqrt(n)

Results = matrix(nrow=R, ncol=3)
t_start = Sys.time()
pb = txtProgressBar(style = 3)

for(r in 1:R){
  data = MainDGP(n=500,p=1000,Ry=.5,Rd=.2, rho=.5,a=.5)
  y = data$y; X = data$X; d = data$d
  Xtilde = cbind(X,d)
  
  # 1. Lasso in full model
  fit = LassoFISTA(betaInit=rep(0,ncol(Xtilde)),y,Xtilde,W=rep(1,nrow(Xtilde)),
                   nopen=c(1,ncol(Xtilde)),lambda,
                   tol=1e-8,maxIter=1000,trace=T)
  betalasso = fit$beta
  
  # 2. Post-Lasso
  shat = which(betalasso[1:p]!=0)
  olsfit = lm(y ~ d + X[,shat])
  
  
  # 3. Double selection
  outcomefit = LassoFISTA(betaInit=rep(0,ncol(X)),y,X,W=rep(1,nrow(X)),
                   nopen=c(1,ncol(X)),lambda,
                   tol=1e-8,maxIter=1000,trace=T)
  treatmentfit = LassoFISTA(betaInit=rep(0,ncol(X)),d,X,W=rep(1,nrow(X)),
                          nopen=c(1,ncol(X)),lambda/2,
                          tol=1e-8,maxIter=1000,trace=T) 
  shatDS = union(which(outcomefit$beta!=0),which(treatmentfit$beta!=0))
  
  DSfit = lm(y ~ d + X[,shatDS])
  
  Results[r,] = c(betalasso[1002],coef(olsfit)['d'],coef(DSfit)['d'])
  setTxtProgressBar(pb, r/R)
}

close(pb)
print(Sys.time()-t_start)

### Compute statistics
StatDisplay = data.frame()
StatDisplay[1:3,"bias"] = abs(apply(Results-.5,2,mean))
StatDisplay[1:3,"sd"]  = apply(Results,2,sd)
StatDisplay[1:3,"RMSE"]  = sqrt(apply((Results-.5)^2,2,mean))

### Function for plot
get.plot <- function(data,modelS,title="A Title",sdBCH){
  plot_res <- ggplot(subset(data, (model==modelS)), aes(x=val)) + 
    geom_histogram(binwidth = .02, alpha=.5, position='identity',fill="steelblue", aes(y = ..density..)) +
    scale_x_continuous( name="Treatment effect") +
    ggtitle(title) + 
    stat_function(fun = dnorm, args=list(mean=.5, sd=sdBCH), colour="darkorchid3", size=1) +
    theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position="none")
  
  return(plot_res)
}

id = c(mapply(function(x) rep(x,R),1:3))
val = c(Results)
data_res = data.frame(val = val, model = id)

grid.arrange(get.plot(data_res,1,"Lasso", sd(Results[,1])), get.plot(data_res,2,"Post-Lasso", sd(Results[,2])),
             get.plot(data_res,3,"Double Selection", sd(Results[,3])),
             ncol=3,nrow=1)


