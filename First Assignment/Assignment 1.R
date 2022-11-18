## a) Posterior distribution for Lambda

curve(dgamma(x,21+x,6+1), to = 10)

## b) Number of requests for the 7th hour
lambda <- 21/6 # this will not depend on lambda, hence lambda can take any value
k <- 0:10
barplot(dpois(k,lambda)*dgamma(lambda,21,6)/dgamma(lambda,21+k,7), names.arg = k)

## c)
p<-seq(1, 10, length.out=10*10)
a <- dgamma(p, 21, 6) # prior
b <- dpois(2,p)*dpois(6,p)*dpois(3,p)*dpois(4,p)*dpois(3,p)*dpois(3,p) # likelihood
c <- a*b/sum(a*b)
barplot(c,names.arg = format(round(p, 2), nsmall = 2))

## d)
a_new <- dnorm(p, 2.3, 1.7) # new prior
c <- a_new*b/sum(a_new*b)
barplot(c,names.arg = format(round(p, 2), nsmall = 2))

## e)
f1 <- function(lambda) {dpois(3,lambda)*(dpois(3,lambda)*dpois(5,lambda)*dpois(3,lambda)*dpois(4,lambda)*dpois(3,lambda)*dpois(3,lambda))*dnorm(lambda,2.3,1.7)}
f2 <- function(lambda) {(dpois(3,lambda)*dpois(5,lambda)*dpois(3,lambda)*dpois(4,lambda)*dpois(3,lambda)*dpois(3,lambda))*dnorm(lambda,2.3,1.7)}

i1=integrate(Vectorize(f1), 0, Inf)$value
i2=integrate(Vectorize(f2), 0, Inf)$value

i1/i2

## f)

h <- c()
for (x in 1:10000) {
  lambda = sample(p, 1, prob=c)
  h[x] = rpois(1, lambda) 
}
hist(h)
