## a) Posterior distribution for Lambda

curve(dgamma(x, 21, 6), to = 10)

## b) Number of requests for the 7th hour

# This will not depend on lambda, hence lambda can take any value
lambda <- 21/6
curve(dpois(x, lambda)*dgamma(x,21,6)/dgamma(x,21+x,))
prior <- dpois(x, lambda)
 <- dgamma()
prediction <- 

## c)
p <- sec(0, 10, length.out = 11)
a <- dgamma(p, 21, 6)
b <- dpois(4,p)
c <- a*b/sum(a*b)
d <- dpois()
