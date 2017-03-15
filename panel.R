n=1000
d = runif(n) > .5
y = rnorm(n) + 1*d

mean(y[d==1])-mean(y[d==0])
summary(lm(y ~ d))

pi = mean(d)

denom = sum((d-pi)^2)
num = sum((d-pi)*y)

### Random effect panel data
n = 1000
H = 4
X = rnorm(n*H)
a = rnorm(n)
a = rep(a,each=H)

y = 1*X + a + rnorm(n*H)

summary(lm(y ~ X))