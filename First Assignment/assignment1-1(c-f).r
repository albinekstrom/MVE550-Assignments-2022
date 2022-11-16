#1c
p<-seq(1, 10,length.out=19*2)
k<-p
a<-dgamma(p,21,6) # prior
b<-dpois(3,p)*dpois(5,p)*dpois(3,p)*dpois(4,p)*dpois(3,p)*dpois(3,p) # likelihood
c<-a*b/sum(a*b)
barplot(c,names.arg = format(round(p, 2), nsmall = 2))

#d
p<-seq(1, 10,length.out=19*2)
a<-dnorm(p,2.3,1.7) # prior
b<-dpois(3,p)*dpois(5,p)*dpois(3,p)*dpois(4,p)*dpois(3,p)*dpois(3,p) # likelihood (how do we get y?)
c<-a*b/sum(a*b)
barplot(c,names.arg = format(round(p, 2), nsmall = 2))

#e
f1 <- function(lambda) {dpois(3,lambda)*(dpois(3,lambda)*dpois(5,lambda)*dpois(3,lambda)*dpois(4,lambda)*dpois(3,lambda)*dpois(3,lambda))*dnorm(lambda,2.3,1.7)}
f2 <- function(lambda) {(dpois(3,lambda)*dpois(5,lambda)*dpois(3,lambda)*dpois(4,lambda)*dpois(3,lambda)*dpois(3,lambda))*dnorm(lambda,2.3,1.7)}

i1=integrate(Vectorize(f1), 0, Inf)$value
i2=integrate(Vectorize(f2), 0, Inf)$value

i1/i2


#f
p<-seq(1, 10,length.out=19*2)
a<-dnorm(p,2.3,1.7) # prior
b<-dpois(3,p)*dpois(5,p)*dpois(3,p)*dpois(4,p)*dpois(3,p)*dpois(3,p) # likelihood (how do we get y?)
c<-a*b/sum(a*b)

h<-rpois(10000,sample(p,5,prob=c))
hist(h)

