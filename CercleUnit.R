### Draw a point on unit circle
### Jeremy L Hour
### 14/06/2017

### marginal density of coordinate on x-axis
densx <- function(x,r=1){
  if(x^2 > r^2){
    return(0)
  } else {
    return(2*sqrt(r^2-x^2)/(pi*r^2))
  }
}

Xsupp = seq(-1.2,1.2,by=.01)

plot(Xsupp,mapply(densx,Xsupp), type="l")

I1 = integrate(densx,-1,1)

### Simulate draws
rUnitCircle <- function(n=100,rad=1){ 
  u = 2*pi*runif(n) 
  r = sqrt(runif(n)) 
  rad*cbind(x=r*cos(u), y=r*sin(u)) 
} 

data = rUnitCircle(100000,rad=1)

print(mean(data[,1]^2+data[,2]^2))