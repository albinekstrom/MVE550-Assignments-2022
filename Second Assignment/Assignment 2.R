# Data
y=c(1,2,3,2,0,0,1,4,2)

# Functions
G <- Vectorize(function(s,lambda) 
{
  a=dpois(0:10,lambda)
  sum(a[1:length(a)]*s^(0:(length(a)-1)))
})

a <- function(lambda) {dpois(0:10,lambda)} # Child dist.

get_extintion_p = Vectorize(function(lambda) {
  optimize(Vectorize(function(x) {abs(G(x,lambda)-x)}), interval =c(0,0.99))$minimum
})

posterior <-function(x) {dgamma(x,sum(y),9)}


#2.a) Plotting the posterior of lambda
curve(dgamma(x,sum(y),9),to=5)

#2.b) Plotting the rate of extinction based on different values of lambda = x in the curve
curve(get_extintion_p(x), to = 10)

#2.c)

g_ext <- function(p) posterior(p)*get_extintion_p(p)


integrate(Vectorize(g_ext), 0, Inf)$value


#2.d)
p <- seq(0, 10, length.out=500)
ext_list = c()

# Sample extinctions
for (i in 1:1000){
  lambda_sample <- sample(p, 1, prob = posterior(p))
  ext_list[i]=get_extintion_p(lambda_sample)
}

# avg. extinction | y
mean(ext_list)

#2.e
get_lh <- function(x)
{
  likelyhood = 1
  for (i in 1:length(y))
  {
    likelyhood = likelyhood*dpois(y[i],x)
  }
  return (likelyhood)
}

max_lambda = optimize(Vectorize(get_lh),c(0,10),maximum = T)$maximum
get_extintion_p(max_lambda)
